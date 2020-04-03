package name.aloise.bson

import cats.data.{Validated, ValidatedNec}
import mercator.Monadic
import magnolia.CaseClass
import name.aloise.bson.Decoder.DecodedResult
import cats.data.Validated._
import name.aloise.bson.utils.FieldMappings
import org.bson.BsonValue
import org.mongodb.scala.bson.BsonDocument

sealed trait DecoderError
case class FieldWasNotFoundInBsonDocument(bson: BsonDocument, mappedFieldName: String, fieldName: String) extends DecoderError
case class UnableToDecodeBsonAsCaseClass(bson: BsonValue, className: String) extends DecoderError

trait Decoder[+T] {
  def apply(a: BsonValue): DecodedResult[T]
}

object Decoder {
  type DecodedResult[+T] = ValidatedNec[DecoderError, T]

  def apply[T : Decoder](a: BsonValue): DecodedResult[T] = implicitly[Decoder[T]].apply(a)
}

object DecoderDerivation extends FieldMappings {
  type Typeclass[T] = Decoder[T]

  protected implicit def monadicValidated[T]: Monadic[DecodedResult] = ???

  def combine[T](caseClass: CaseClass[Typeclass, T])(
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
              validNec(p.default.get)
            case _ =>
              invalidNec(FieldWasNotFoundInBsonDocument(doc, key, p.label))
          }
        case _ =>
          invalidNec(UnableToDecodeBsonAsCaseClass(b, caseClass.typeName.full))
      }
    }
  }
}