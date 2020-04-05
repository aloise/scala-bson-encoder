package name.aloise.bson.derivation

import magnolia.Magnolia
import name.aloise.bson.{BsonDecoder, BsonDecoderDerivation, BsonEncoder}
import scala.language.experimental.macros

object decoder {
  object auto extends BsonDecoderDerivation {
    type Typeclass[T] = BsonDecoder[T]
    implicit def decoder[T]: Typeclass[T] = macro Magnolia.gen[T]
  }

  object semiauto extends BsonDecoderDerivation {
    type Typeclass[T] = BsonEncoder[T]
    def deriveDecoder[T]: Typeclass[T] = macro Magnolia.gen[T]
  }
}
