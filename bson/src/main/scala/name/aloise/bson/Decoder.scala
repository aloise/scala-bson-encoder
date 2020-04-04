package name.aloise.bson

import java.time.{Instant, LocalDate, LocalDateTime, LocalTime, ZoneId}

import cats.{Applicative, Functor}
import cats.data.Validated._
import cats.data.ValidatedNec
import cats.kernel.Monoid
import magnolia.{CaseClass, Magnolia, SealedTrait}
import mercator.Monadic
import name.aloise.bson.Decoder.DecodedResult
import name.aloise.bson.utils.FieldMappings
import org.bson._

import scala.collection.IterableFactory
import scala.language.experimental.macros
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


trait Decoder[T] {
  def apply(a: BsonValue): DecodedResult[T]
}

trait LowestPrioDecoders {

  implicit val applicative: Functor[Decoder] = new Functor[Decoder] {
    override def map[A, B](fa: Decoder[A])(f: A => B): Decoder[B] = (a: BsonValue) => fa(a).map(f)
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

  protected def primitiveDecoder[T <: BsonValue { def getValue(): V } : ClassTag, V : ClassTag]: Decoder[V] = {
    case value: T => valid(value.getValue())
    case other => invalidNec(PrimitiveDecoderFailed(other, implicitly[ClassTag[T]].runtimeClass.getName))
  }

  implicit val stringDecoder: Decoder[String] = primitiveDecoder[BsonString, String]
  implicit val intDecoder: Decoder[Int] = primitiveDecoder[BsonInt32, Int]

  implicit val longDecoder: Decoder[Long] = {
    case value: BsonInt32 => valid(value.getValue)
    case value: BsonInt64 => valid(value.getValue)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Long].getName))
  }

  implicit val doubleDecoder: Decoder[Double] = primitiveDecoder[BsonDouble, Double]

  implicit val localDateTimeDecoder: Decoder[LocalDateTime] = {
    case t: BsonDateTime => valid(Instant.ofEpochMilli(t.getValue).atZone(ZoneId.of("UTC")).toLocalDateTime)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[LocalDateTime].getName))
  }

  implicit val instantDecoder: Decoder[Instant] = {
    case t: BsonDateTime => valid(Instant.ofEpochMilli(t.getValue)) // time in millis
    case t: BsonTimestamp => valid(Instant.ofEpochMilli(t.getValue*1000)) // time in seconds
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[LocalDateTime].getName))
  }

  implicit val localDateDecoder: Decoder[LocalDate] = localDateTimeDecoder.map(_.toLocalDate)
  implicit val localTimeDecoder: Decoder[LocalTime] = localDateTimeDecoder.map(_.toLocalTime)

  implicit val bigDecimalDecoder: Decoder[BigDecimal] = {
    case str: BsonNumber => valid(BigDecimal(str.decimal128Value.bigDecimalValue()))
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[BigDecimal].getName))
  }

  implicit val boolDecoder: Decoder[Boolean] = primitiveDecoder[BsonBoolean, Boolean]

  implicit val byteArrayDecoder: Decoder[Array[Byte]] = {
    case blob: BsonBinary => valid(blob.getData)
    case bson => invalidNec(PrimitiveDecoderFailed(bson, classOf[Array[Byte]].getName))
  }

  implicit def optionDecoder[A : Decoder]: Decoder[Option[A]] = {
    case _: BsonNull => valid(None)
    case value: BsonValue => Decoder[A](value).map(Option(_))
  }

  implicit def listDecoder[A : Decoder]: Decoder[List[A]] = {
    case arr: BsonArray =>
      import cats.implicits._
      import scala.jdk.CollectionConverters._
      arr.getValues.asScala.map(Decoder[A]).toList.sequence
    case value => invalidNec(BsonArrayDecoderFailed(value))
  }

  implicit def setDecoder[A : Decoder]: Decoder[Set[A]] = listDecoder[A].map(_.toSet)
  implicit def vectorDecoder[A : Decoder]: Decoder[Vector[A]] = listDecoder[A].map(_.toVector)

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