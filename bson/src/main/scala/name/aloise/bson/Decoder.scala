package name.aloise.bson

import cats.data.Validated
import mercator.Monadic
import magnolia.{CaseClass, Magnolia, SealedTrait}
import name.aloise.bson.Decoder.DecodedResult
import cats.data.Validated._
import name.aloise.bson.utils.FieldMappings
import org.bson._

import scala.language.experimental.macros

sealed trait DecoderError
trait AdtDecodeError extends DecoderError
case class FieldWasNotFoundInBsonDocument(bson: BsonDocument, mappedFieldName: String, fieldName: String) extends DecoderError
case class UnableToDecodeBsonAsCaseClass(bson: BsonValue, className: String) extends DecoderError
case class DiscriminatorFieldIsMissing(bson: BsonDocument, discriminatorFieldName: String) extends AdtDecodeError
case class DiscriminatorFieldContainsNonStringValue(bson: BsonDocument, discriminatorFieldName: String, discriminatorFieldValue: BsonValue) extends AdtDecodeError
case class AdtCaseClassCanNotBeDecodedFromBsonValue(bson: BsonValue, sealedTraitName: String) extends AdtDecodeError
case class AdtCaseClassNameWasNotFound(caseClassName: String, knownCaseClassNames: Set[String]) extends AdtDecodeError

trait Decoder[+T] {
  def apply(a: BsonValue): DecodedResult[T]
}

trait LowPrioDecoders {
  implicit class FromBson(bson: BsonValue) {
    def fromBson[T : Decoder]: DecodedResult[T] = Decoder[T](bson)
  }
}

object Decoder extends LowPrioDecoders {
  type DecodedResult[+T] = Validated[DecoderError, T]
  def apply[T : Decoder](a: BsonValue): DecodedResult[T] = implicitly[Decoder[T]].apply(a)
}

object DecoderDerivation extends FieldMappings {
  type Typeclass[T] = Decoder[T]

  protected implicit def monadicValidated[T]: Monadic[DecodedResult] = new Monadic[DecodedResult] {
    override def point[A](value: A): DecodedResult[A] = valid(value)

    override def flatMap[A, B](from: DecodedResult[A])(fn: A => DecodedResult[B]): DecodedResult[B] =
      from match {
        case Valid(value) => fn(value)
        case Invalid(err) => invalid(err)
      }

    override def map[A, B](from: DecodedResult[A])(fn: A => B): DecodedResult[B] = from.map(fn)
  }

  private def combine[T](caseClass: CaseClass[Typeclass, T])(
    implicit config: Configuration): Typeclass[T] = {
    val paramsLookup = getFieldNameMappings[Typeclass, T](caseClass)
    (bson: BsonValue) =>
    caseClass.constructMonadic { p =>
      val key = paramsLookup(p.label)
      bson match {
        case b: BsonValue if caseClass.isValueClass =>
          Decoder[p.PType](b)(p.typeclass)
        case doc: BsonDocument =>
          Option(doc.get(key)) match {
            case Some(value) =>
              Decoder[p.PType](value)(p.typeclass)
            case None if p.default.isDefined =>
              valid(p.default.get)
            case _ =>
              invalid(FieldWasNotFoundInBsonDocument(doc, key, p.label))
          }
        case _ =>
          invalid(UnableToDecodeBsonAsCaseClass(bson, caseClass.typeName.full))
      }
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[Typeclass, T])(implicit config: Configuration): Typeclass[T] = {
    val lookup = constructorLookup[Typeclass, T](sealedTrait)
    (bson: BsonValue) => bson match {
      case doc: BsonDocument if doc.containsKey(config.discriminatorFieldName) =>
        doc.get(config.discriminatorFieldName) match {
          case str: BsonString if lookup.contains(str.getValue) =>
            val constructor = lookup(str.getValue)
            Decoder[constructor.SType](doc)(constructor.typeclass) match {
              case err@Invalid(_) =>
                // Trying to decode the primitive value
                // TODO - find a better way to encode primitive values from custom case class encoders
                Option(doc.get(config.adtPrimitiveValueFieldName)) match {
                  case None => err //
                  case Some(value) => Decoder[constructor.SType](value)(constructor.typeclass)
                }
              case valid => valid
            }
          case str: BsonString =>
            invalid(AdtCaseClassNameWasNotFound(str.getValue, lookup.keySet))
          case other =>
            invalid(DiscriminatorFieldContainsNonStringValue(doc, config.discriminatorFieldName, other))
        }
      case doc: BsonDocument =>
        invalid(DiscriminatorFieldIsMissing(doc, config.discriminatorFieldName))
      case _ =>
        invalid(AdtCaseClassCanNotBeDecodedFromBsonValue(bson, sealedTrait.typeName.full))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}