package name.aloise.bson

import cats.data.Validated._
import cats.data.ValidatedNec
import magnolia.{CaseClass, Magnolia, SealedTrait}
import mercator.Monadic
import name.aloise.bson.Decoder.DecodedResult
import name.aloise.bson.utils.FieldMappings
import org.bson._

import scala.collection.IterableFactory
import scala.language.experimental.macros

sealed trait DecoderError
trait AdtDecodeError extends DecoderError
case class PrimitiveDecoderFailed(b: BsonValue, primitiveTypeClassName: String) extends DecoderError
case class BsonArrayDecoderFailed(notBsonArray: BsonValue) extends DecoderError
case class FieldWasNotFoundInBsonDocument(bson: BsonDocument, mappedFieldName: String, fieldName: String) extends DecoderError
case class UnableToDecodeBsonAsCaseClass(bson: BsonValue, className: String) extends DecoderError
case class DiscriminatorFieldIsMissing(bson: BsonDocument, discriminatorFieldName: String) extends AdtDecodeError
case class DiscriminatorFieldContainsNonStringValue(bson: BsonDocument, discriminatorFieldName: String, discriminatorFieldValue: BsonValue) extends AdtDecodeError
case class AdtCaseClassCanNotBeDecodedFromBsonValue(bson: BsonValue, sealedTraitName: String) extends AdtDecodeError
case class AdtCaseClassNameWasNotFound(caseClassName: String, knownCaseClassNames: Set[String]) extends AdtDecodeError


trait Decoder[+T] {
  def apply(a: BsonValue): DecodedResult[T]
}

trait LowestPrioDecoders {
  import scala.jdk.CollectionConverters._
  implicit def iterableDecoder[T[_] : IterableFactory, A : Decoder]: Decoder[T[A]] = {
    case arr: BsonArray =>
      import cats.implicits._

      val mapped: DecodedResult[List[A]] = arr.getValues.asScala.map(Decoder[A]).toList.sequence
      mapped.map {
        implicitly[IterableFactory[T]].from
      }
    case value => invalidNec(BsonArrayDecoderFailed(value))
  }
}

trait LowPrioDecoders extends LowestPrioDecoders {


  implicit val stringDecoder: Decoder[String] = {
    case value: BsonString => valid(value.getValue)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[String].getName))
  }
  implicit val intDecoder: Decoder[Int] = {
    case value: BsonInt32 => valid(value.getValue)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Int].getName))
  }
  implicit val longDecoder: Decoder[Long] = {
    case value: BsonInt32 => valid(value.getValue)
    case value: BsonInt64 => valid(value.getValue)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Long].getName))
  }
  implicit val doubleDecoder: Decoder[Double] = {
    case str: BsonNumber => valid(str.doubleValue())
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Double].getName))
  }

  implicit val bigDecimalDecoder: Decoder[BigDecimal] = {
    case str: BsonNumber => valid(BigDecimal(str.decimal128Value.bigDecimalValue()))
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[BigDecimal].getName))
  }

  implicit val boolDecoder: Decoder[Boolean] = {
    case value: BsonBoolean => valid(value.getValue)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Boolean].getName))
  }

  implicit val byteArrayDecoder: Decoder[Array[Byte]] = {
    case blob: BsonBinary => valid(blob.getData)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Array[Byte]].getName))
  }

  implicit def optionDecoder[A : Decoder]: Decoder[Option[A]] = {
    case _: BsonNull => valid(None)
    case value: BsonValue => Decoder[A](value).map(Option(_))
  }

}

object Decoder extends LowPrioDecoders with FieldMappings {

  implicit class FromBson(bson: BsonValue) {
    def as[T : Decoder]: DecodedResult[T] = Decoder[T](bson)
  }

  type DecodedResult[+T] = ValidatedNec[DecoderError, T]

  def apply[T : Decoder](a: BsonValue): DecodedResult[T] = implicitly[Decoder[T]].apply(a)

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

  def combine[T](caseClass: CaseClass[Typeclass, T])(implicit config: Configuration): Typeclass[T] = {
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
              invalidNec(FieldWasNotFoundInBsonDocument(doc, key, p.label))
          }
        case _ =>
          invalidNec(UnableToDecodeBsonAsCaseClass(bson, caseClass.typeName.full))
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
            invalidNec(AdtCaseClassNameWasNotFound(str.getValue, lookup.keySet))
          case other =>
            invalidNec(DiscriminatorFieldContainsNonStringValue(doc, config.discriminatorFieldName, other))
        }
      case doc: BsonDocument =>
        invalidNec(DiscriminatorFieldIsMissing(doc, config.discriminatorFieldName))
      case _ =>
        invalidNec(AdtCaseClassCanNotBeDecodedFromBsonValue(bson, sealedTrait.typeName.full))
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}