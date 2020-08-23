package name.aloise.bson

import java.time.{LocalDate, LocalDateTime, ZoneId}

import cats.Contravariant
import org.bson.BsonValue
import magnolia._
import name.aloise.bson.utils.FieldMappings
import org.bson._

import scala.language.experimental.macros


trait BsonEncoder[T] {
  def apply(a: T): BsonValue
}

object BsonEncoder {
  implicit class ToBson[T : BsonEncoder](a: T) {
    import scala.jdk.CollectionConverters._

    def toBson: BsonValue = BsonEncoder[T](a)

    def toDocument: org.bson.Document = toBson match {
      case doc: BsonDocument =>
        new org.bson.Document(doc.asScala.map{case (k, v) => (k, v: Object)}.asJava)
      case b => throw new IllegalArgumentException(s"Can't encode Bson value of type ${b.getBsonType.name} as BsonDocument")
    }
  }

  def apply[T : BsonEncoder](a: T): BsonValue = implicitly[BsonEncoder[T]].apply(a)



  protected def document(elems: EncodeToBsonElement*): BsonDocument = {
    val underlying = new BsonDocument()
    elems.foreach(elem => underlying.put(elem.key, elem.value))
    underlying
  }

  protected def array(elems: EncodeToBsonValue*): BsonArray = {
    val underlying = new BsonArray()
    elems.foreach(e => underlying.add(e.value))
    underlying
  }
}


trait LowestPrioEncoders {
  import scala.jdk.CollectionConverters._
  implicit def bsonValueEncoder[T <: BsonValue]: BsonEncoder[T] = (bson: T) => bson

  implicit val encoderFunctor: Contravariant[BsonEncoder]= new Contravariant[BsonEncoder] {
    override def contramap[A, B](fa: BsonEncoder[A])(f: B => A): BsonEncoder[B] = (a: B) => fa(f(a))
  }
  implicit def iterableEncoder[CC[A] <: Iterable[A], A : BsonEncoder]: BsonEncoder[CC[A]] = (a: Iterable[A]) =>
    new BsonArray(a.map(BsonEncoder[A]).toList.asJava)
}

trait LowPrioEncoders extends LowestPrioEncoders {
  import cats.implicits._
  import scala.jdk.CollectionConverters._

  implicit val stringEncoder: BsonEncoder[String] = new BsonString(_)
  implicit val intEncoder: BsonEncoder[Int] = new BsonInt32(_)
  implicit val longEncoder: BsonEncoder[Long] = new BsonInt64(_)
  implicit val doubleEncoder: BsonEncoder[Double] = new BsonDouble(_)
  implicit val localDateTimeEncoder: BsonEncoder[LocalDateTime] = (d: LocalDateTime) => new BsonDateTime(d.atZone(ZoneId.of("UTC")).toInstant.toEpochMilli)
  implicit val localDateEncoder: BsonEncoder[LocalDate] = localDateTimeEncoder.contramap(LocalDateTime.from)
  implicit val boolEncoder: BsonEncoder[Boolean] = new BsonBoolean(_)
  implicit val byteArrayEncoder: BsonEncoder[Array[Byte]] = new BsonBinary(_)

  implicit def optionEncoder[A : BsonEncoder]: BsonEncoder[Option[A]] = (a: Option[A]) =>
    a.fold[BsonValue](BsonExclude)(BsonEncoder[A])

  implicit def arrayEncoder[A : BsonEncoder]: BsonEncoder[Array[A]] = (a: Array[A]) =>
    new BsonArray(a.iterator.map(BsonEncoder[A]).toList.asJava)

  implicit def listEncoder[A : BsonEncoder]: BsonEncoder[List[A]] = iterableEncoder[List, A]

}

trait BsonEncoderDerivation extends LowPrioEncoders with FieldMappings {

  protected def addFieldToDocument(baseDoc: BsonDocument, fieldName: String, value: BsonValue): BsonDocument = {
    if(value != BsonExclude && value != null) {
      baseDoc.put(fieldName, value)
    }
    baseDoc
  }

  def combine[T](caseClass: CaseClass[BsonEncoder, T])(implicit config: Configuration): BsonEncoder[T] = {
    val paramsLookup = getFieldNameMappings[BsonEncoder, T](caseClass)

    (a: T) => {
      if (caseClass.isValueClass) {
        // encode it as an internal value
        val param = caseClass.parameters.head
        param.typeclass(param.dereference(a))
      } else {
        val result =
          caseClass.parameters.foldLeft(new BsonDocument()) { case (doc, p) =>
            val label = paramsLookup(p.label)
            val encodedBson = p.typeclass(p.dereference(a))
            addFieldToDocument(doc, label, encodedBson)
          }
        if(caseClass.isObject) {
          addFieldToDocument(result, config.discriminatorFieldName, new BsonString(caseClass.typeName.short))
        } else result
      }
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[BsonEncoder, T])(implicit config: Configuration): BsonEncoder[T] = {
    constructorLookup(sealedTrait)
    (a: T) =>
      sealedTrait.dispatch(a) { subtype =>
        val baseBson = subtype.typeclass(subtype.cast(a))
        val className = config.adtClassNameMapper(subtype.typeName.short)
        val outputDocWithoutTypeDiscriminator =
          baseBson match {
            case doc: BsonDocument if doc.containsKey(config.discriminatorFieldName) =>
              // It was encoded as a document, we are injecting the discriminator field
              // field name clashes with discriminator field
              throw new IllegalArgumentException(
                s"Can't inject the discriminator field '${config.discriminatorFieldName}'. The field with the same name already exists in the object.")

            case doc: BsonDocument =>
              doc

            case _ =>
              // We cant' inject the discriminator field cause it's not a BsonDocument. The discriminator field is wrapped in the object
              addFieldToDocument(new BsonDocument(), config.adtPrimitiveValueFieldName, baseBson)
          }
        addFieldToDocument(outputDocWithoutTypeDiscriminator, config.discriminatorFieldName, new BsonString(className))
      }
  }

//  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

sealed trait EncodeToBsonValue {
  def value: BsonValue
}

object EncodeToBsonValue {
  implicit def encodeToBsonValue[T : BsonEncoder](v: T): EncodeToBsonValue = new EncodeToBsonValue {
    val value: BsonValue = BsonEncoder[T](v)
  }
}

sealed trait EncodeToBsonElement extends EncodeToBsonValue {
  val bsonElement: BsonElement
  def key: String = bsonElement.getName
  def value: BsonValue = bsonElement.getValue
}

object EncodeToBsonElement {
  implicit def tupleToCanBeBsonElement[T : BsonEncoder](kv: (String, T)): EncodeToBsonElement = {
    new EncodeToBsonElement {
      override val bsonElement: BsonElement = new BsonElement(kv._1, BsonEncoder[T](kv._2))
    }
  }
}
