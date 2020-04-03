package name.aloise.bson

import cats.data.{Validated, ValidatedNec}
import mercator.Monadic
import magnolia.{CaseClass, Magnolia}
import name.aloise.bson.Decoder.DecodedResult
import cats.data.Validated._
import name.aloise.bson.utils.FieldMappings
import org.bson.BsonValue
import org.mongodb.scala.bson.BsonDocument
import scala.language.experimental.macros

sealed trait DecoderError
case class FieldWasNotFoundInBsonDocument(bson: BsonDocument, mappedFieldName: String, fieldName: String) extends DecoderError
case class UnableToDecodeBsonAsCaseClass(bson: BsonValue, className: String) extends DecoderError

trait Decoder[+T] {
  def apply(a: BsonValue): DecodedResult[T]
}

object Decoder {
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
    (b: BsonValue) =>
    caseClass.constructMonadic { p =>
      val key = paramsLookup(p.label)
      b match {
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
          invalid(UnableToDecodeBsonAsCaseClass(b, caseClass.typeName.full))
      }
    }
  }

  implicit def gen[T]: Typeclass[T] = macro Magnolia.gen[T]
}