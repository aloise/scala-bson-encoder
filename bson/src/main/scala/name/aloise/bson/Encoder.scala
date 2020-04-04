package name.aloise.bson

import org.bson.BsonValue
import magnolia._
import name.aloise.bson.utils.FieldMappings
import org.bson._
import scala.language.experimental.macros


trait Encoder[-T] {
  def apply(a: T): BsonValue
}

trait LowPrioEncoders {

  implicit class ToBson[T : Encoder](a: T) {
    def toBson: BsonValue = Encoder[T](a)
  }

//  import shapeless._
  import scala.jdk.CollectionConverters._

  implicit val stringEncoder: Encoder[String] = new BsonString(_)
  implicit val intEncoder: Encoder[Int] = new BsonInt32(_)
  implicit val longEncoder: Encoder[Long] = new BsonInt64(_)
  implicit val doubleEncoder: Encoder[Double] = new BsonDouble(_)
  implicit val boolEncoder: Encoder[Boolean] = new BsonBoolean(_)
  implicit val byteArrayEncoder: Encoder[Array[Byte]] = new BsonBinary(_)
  implicit def optionEncoder[A : Encoder]: Encoder[Option[A]] = (a: Option[A]) =>
    a.fold[BsonValue](new BsonNull)(Encoder[A])
  implicit def iterableEncoder[A : Encoder]: Encoder[Iterable[A]] = (a: Iterable[A]) =>
    new BsonArray(a.map(Encoder[A]).toList.asJava)
}

object Encoder extends LowPrioEncoders with FieldMappings {

  def apply[T : Encoder](a: T): BsonValue = implicitly[Encoder[T]].apply(a)

  type Typeclass[T] = Encoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T])(
    implicit config: Configuration): Typeclass[T] = {
    val paramsLookup = getFieldNameMappings[Typeclass, T](caseClass)

    (a: T) => {
      if (caseClass.isValueClass) {
        // encode it as an internal value
        val param = caseClass.parameters.head
        param.typeclass(param.dereference(a))
      } else {
        val doc = new BsonDocument()
        caseClass.parameters.foreach { p =>
          val label = paramsLookup(p.label)
          doc.put(label, p.typeclass(p.dereference(a)))
        }
        doc
      }
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T])(implicit config: Configuration): Typeclass[T] = {
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
              val wrappingDoc = new BsonDocument()
              wrappingDoc.put(config.adtPrimitiveValueFieldName, baseBson)
              wrappingDoc
          }
        outputDocWithoutTypeDiscriminator.put(config.discriminatorFieldName, new BsonString(className))
        outputDocWithoutTypeDiscriminator
      }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

