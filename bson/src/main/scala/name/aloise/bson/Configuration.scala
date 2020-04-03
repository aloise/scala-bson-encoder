package name.aloise.bson

import scala.annotation.StaticAnnotation

case class Configuration(
    fieldNameMapper: String => String = identity,
    discriminatorFieldName: String,
    adtPrimitiveValueFieldName: String,
    adtClassNameMapper: String => String = identity
)

final case class Key(value: String) extends StaticAnnotation

object Configuration {
  implicit val DefaultBsonConfiguration: Configuration =
    Configuration(discriminatorFieldName = "className",
      adtPrimitiveValueFieldName = "value")
}