package name.aloise.bson

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneId}

import cats.Functor
import cats.data.Validated._
import cats.data.ValidatedNec
import magnolia.{CaseClass, SealedTrait}
import mercator.Monadic
import name.aloise.bson.BsonDecoder.DecodedResult
import name.aloise.bson.utils.FieldMappings
import org.bson._
import scala.reflect.ClassTag

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


trait BsonDecoder[T] {
  /**
   * What to do in key was with this decoder was not found in the Document. Example: Option : None
   * @return
   */
  def defaultOnNotFoundKey: Option[T] = None
  def apply(a: BsonValue): DecodedResult[T]
}

trait LowestPrioDecoders {

  implicit val applicative: Functor[BsonDecoder] = new Functor[BsonDecoder] {
    override def map[A, B](fa: BsonDecoder[A])(f: A => B): BsonDecoder[B] = (a: BsonValue) => fa(a).map(f)
  }

  import scala.jdk.CollectionConverters._

  // TODO - doesn't work
/*  implicit def iterableDecoder[T[_] <: Iterable[A] : IterableFactory, A : Decoder]: Decoder[T[A]] = {
    case arr: BsonArray =>
      import cats.implicits._

      val mapped: DecodedResult[List[A]] = arr.getValues.asScala.map(Decoder[A]).toList.sequence
      mapped.map {
        implicitly[IterableFactory[T]].from
      }
    case value => invalidNec(BsonArrayDecoderFailed(value))
  }*/



}

trait LowPrioDecoders extends LowestPrioDecoders {
  import cats.implicits._

  protected def primitiveDecoder[T <: BsonValue { def getValue(): V } : ClassTag, V : ClassTag]: BsonDecoder[V] = {
    case value: T => valid(value.getValue())
    case other => invalidNec(PrimitiveDecoderFailed(other, implicitly[ClassTag[T]].runtimeClass.getName))
  }

  implicit val stringDecoder: BsonDecoder[String] = primitiveDecoder[BsonString, String]
  implicit val intDecoder: BsonDecoder[Int] = primitiveDecoder[BsonInt32, Int]

  implicit val longDecoder: BsonDecoder[Long] = {
    case value: BsonInt32 => valid(value.getValue)
    case value: BsonInt64 => valid(value.getValue)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Long].getName))
  }

  implicit val doubleDecoder: BsonDecoder[Double] = primitiveDecoder[BsonDouble, Double]

  implicit val localDateTimeDecoder: BsonDecoder[LocalDateTime] = {
    case t: BsonDateTime => valid(Instant.ofEpochMilli(t.getValue).atZone(ZoneId.of("UTC")).toLocalDateTime)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[LocalDateTime].getName))
  }

  implicit val instantDecoder: BsonDecoder[Instant] = {
    case t: BsonDateTime => valid(Instant.ofEpochMilli(t.getValue)) // time in millis
    case t: BsonTimestamp => valid(Instant.ofEpochMilli(t.getValue*1000)) // time in seconds
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[LocalDateTime].getName))
  }

  implicit val localDateDecoder: BsonDecoder[LocalDate] = localDateTimeDecoder.map(_.toLocalDate)
  implicit val localTimeDecoder: BsonDecoder[LocalTime] = localDateTimeDecoder.map(_.toLocalTime)

  implicit val bigDecimalDecoder: BsonDecoder[BigDecimal] = {
    case str: BsonNumber => valid(BigDecimal(str.decimal128Value.bigDecimalValue()))
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[BigDecimal].getName))
  }

  implicit val boolDecoder: BsonDecoder[Boolean] = primitiveDecoder[BsonBoolean, Boolean]

  implicit val byteArrayDecoder: BsonDecoder[Array[Byte]] = {
    case blob: BsonBinary => valid(blob.getData)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Array[Byte]].getName))
  }

  protected class OptionDecoder[A : BsonDecoder] extends BsonDecoder[Option[A]] {
    override def defaultOnNotFoundKey: Option[None.type] = Some(None)
    override def apply(a: BsonValue): DecodedResult[Option[A]] = a match {
      case _: BsonNull => valid(None)
      case value: BsonValue => BsonDecoder[A](value).map(Option(_))
    }
  }

  implicit def optionDecoder[A : BsonDecoder]: BsonDecoder[Option[A]] = new OptionDecoder[A]

  implicit def listDecoder[A : BsonDecoder]: BsonDecoder[List[A]] = {
    case arr: BsonArray =>
      import cats.implicits._
      import scala.jdk.CollectionConverters._
      arr.getValues.asScala.map(BsonDecoder[A]).toList.sequence
    case value => invalidNec(BsonArrayDecoderFailed(value))
  }

  implicit def setDecoder[A : BsonDecoder]: BsonDecoder[Set[A]] = listDecoder[A].map(_.toSet)
  implicit def vectorDecoder[A : BsonDecoder]: BsonDecoder[Vector[A]] = listDecoder[A].map(_.toVector)

}

object BsonDecoder {
  implicit class FromBson(bson: BsonValue) {
    def as[T : BsonDecoder]: DecodedResult[T] = BsonDecoder[T](bson)
  }

  type DecodedResult[+T] = ValidatedNec[DecoderError, T]

  def apply[T : BsonDecoder](a: BsonValue): DecodedResult[T] = implicitly[BsonDecoder[T]].apply(a)
}

trait BsonDecoderDerivation extends LowPrioDecoders with FieldMappings {

  protected implicit def monadicValidated[T]: Monadic[DecodedResult] = new Monadic[DecodedResult] {
    override def point[A](value: A): DecodedResult[A] = valid(value)

    override def flatMap[A, B](from: DecodedResult[A])(fn: A => DecodedResult[B]): DecodedResult[B] =
      from match {
        case Valid(value) => fn(value)
        case Invalid(err) => invalid(err)
      }

    override def map[A, B](from: DecodedResult[A])(fn: A => B): DecodedResult[B] = from.map(fn)
  }

  def combine[T](caseClass: CaseClass[BsonDecoder, T])(implicit config: Configuration): BsonDecoder[T] = {
    val paramsLookup = getFieldNameMappings[BsonDecoder, T](caseClass)
    (bson: BsonValue) =>
    caseClass.constructMonadic { p =>
      val key = paramsLookup(p.label)
      bson match {
        case b: BsonValue if caseClass.isValueClass =>
          BsonDecoder[p.PType](b)(p.typeclass)
        case doc: BsonDocument =>
          Option(doc.get(key)) match {
            case Some(value) =>
              BsonDecoder[p.PType](value)(p.typeclass)
            case None if p.default.isDefined =>
              valid(p.default.get)
            case None if p.typeclass.defaultOnNotFoundKey.isDefined =>
              valid(p.typeclass.defaultOnNotFoundKey.get)
            case _ =>
              invalidNec(FieldWasNotFoundInBsonDocument(doc, key, p.label))
          }
        case _ =>
          invalidNec(UnableToDecodeBsonAsCaseClass(bson, caseClass.typeName.full))
      }
    }
  }

  def dispatch[T](sealedTrait: SealedTrait[BsonDecoder, T])(implicit config: Configuration): BsonDecoder[T] = {
    val lookup = constructorLookup[BsonDecoder, T](sealedTrait)
    (bson: BsonValue) => bson match {
      case doc: BsonDocument if doc.containsKey(config.discriminatorFieldName) =>
        doc.get(config.discriminatorFieldName) match {
          case str: BsonString if lookup.contains(str.getValue) =>
            val constructor = lookup(str.getValue)
            BsonDecoder[constructor.SType](doc)(constructor.typeclass) match {
              case err@Invalid(_) =>
                // Trying to decode the primitive value
                // TODO - find a better way to encode primitive values from custom case class encoders
                Option(doc.get(config.adtPrimitiveValueFieldName)) match {
                  case None => err //
                  case Some(value) => BsonDecoder[constructor.SType](value)(constructor.typeclass)
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

}