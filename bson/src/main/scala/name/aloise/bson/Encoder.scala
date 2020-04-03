package name.aloise.bson

import org.bson.BsonValue
import magnolia._
import org.bson._
import scala.language.experimental.macros


trait Encoder[-T] {
  def apply(a: T): BsonValue
}

trait LowPrioEncoders {

  import scala.jdk.CollectionConverters._

  implicit val stringEncoder: Encoder[String] = new BsonString(_)
  implicit val intEncoder: Encoder[Int] = new BsonInt32(_)
  implicit val longEncoder: Encoder[Long] = new BsonInt64(_)
  implicit val doubleEncoder: Encoder[Double] = new BsonDouble(_)
  // implicit def anyValEncoder[T <: AnyVal, H <: T :: HNil]: Encoder[] = ???
  implicit def optionEncoder[A : Encoder]: Encoder[Option[A]] = (a: Option[A]) =>
    a.fold[BsonValue](new BsonNull)(Encoder[A])

  implicit def listEncoder[A : Encoder]: Encoder[Iterable[A]] = (a: Iterable[A]) =>
    new BsonArray(a.map(Encoder[A]).toList.asJava)
}

object Encoder extends LowPrioEncoders {
  def apply[T : Encoder](a: T): BsonValue = implicitly[Encoder[T]].apply(a)
}


object EncoderDerivation {
  type Typeclass[T] = Encoder[T]

  def combine[T](caseClass: CaseClass[Typeclass, T])(
    implicit config: Configuration): Typeclass[T] = {
    val paramsLookup = caseClass.parameters.map { p =>
      val keyAnnotation = p.annotations.collectFirst {
        case ann: Key => ann
      }
      keyAnnotation match {
        case Some(ann) => p.label -> ann.value
        case None      => p.label -> config.fieldNameMapper(p.label)
      }
    }.toMap

    if (paramsLookup.values.toSet.size != caseClass.parameters.length) {
      throw new IllegalStateException(
        s"Duplicate key detected after applying field name mapper function for case class parameters: $paramsLookup"
      )
    }

    (a: T) =>
    {
      val doc = new BsonDocument()
      caseClass.parameters.foreach { p =>
        val label = paramsLookup.getOrElse(
          p.label,
          throw new IllegalStateException(
            "Looking up a parameter label should always yield a value. This is a derivation bug"))
        doc.put(label, p.typeclass(p.dereference(a)))
      }
      doc
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T])(
    implicit config: Configuration): Typeclass[T] = {
    {
      val origTypeNames = sealedTrait.subtypes.map(_.typeName.short)
      val transformed = origTypeNames.map(config.adtClassNameMapper).distinct
      if (transformed.length != origTypeNames.length) {
        throw new IllegalStateException(
          "Duplicate key detected after applying class name mapper function for " +
            "sealed trait child classes"
        )
      }
    }
    (a: T) =>
      sealedTrait.dispatch(a) { subtype =>
        val baseBson = subtype.typeclass(subtype.cast(a))
        val className = config.adtClassNameMapper(subtype.typeName.short)
        val outputDocWithoutTypeDiscriminator =
          baseBson match {
            case doc: BsonDocument
              if (doc.containsKey(config.discriminatorFieldName)) =>
              // It was encoded as a document, we are injecting the discriminator field
              // field name clashes with discriminator field
              throw new IllegalArgumentException(
                s"Can't inject the discriminator field '${config.discriminatorFieldName}'. The field with the same name already exists in the object."
              )

            case doc: BsonDocument =>
              doc

            case _ =>
              // We cant' inject the discriminator field cause it's not a BsonDocument. The discriminator field is wrapped in the object
              val wrappingDoc = new BsonDocument()
              wrappingDoc.put(config.adtPrimitiveValueFieldName, baseBson)
              wrappingDoc
          }
        outputDocWithoutTypeDiscriminator.put(config.discriminatorFieldName,
          new BsonString(className))
        outputDocWithoutTypeDiscriminator
      }

  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}

