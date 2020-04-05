package name.aloise.bson.derivation

import magnolia.Magnolia
import name.aloise.bson.{BsonEncoder, BsonEncoderDerivation}
import scala.language.experimental.macros

object encoder {
  object auto extends BsonEncoderDerivation {
    type Typeclass[T] = BsonEncoder[T]
    implicit def encoder[T]: Typeclass[T] = macro Magnolia.gen[T]
  }

  object semiauto extends BsonEncoderDerivation {
    type Typeclass[T] = BsonEncoder[T]
    def deriveEncoder[T]: Typeclass[T] = macro Magnolia.gen[T]
  }
}
