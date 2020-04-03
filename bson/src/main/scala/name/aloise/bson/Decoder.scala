package name.aloise.bson

import cats.data.ValidatedNec
import name.aloise.bson.Decoder.DecodedResult
import org.bson.BsonValue

sealed trait DecoderError

trait Decoder[+T] {
  def apply(a: BsonValue): DecodedResult[T]
}

object Decoder {
  type DecodedResult[+T] = ValidatedNec[DecoderError, T]

  def apply[T : Decoder](a: BsonValue): DecodedResult[T] = implicitly[Decoder[T]].apply(a)
}

object DecoderDerivation {
  type Typeclass[T] = Decoder[T]
}