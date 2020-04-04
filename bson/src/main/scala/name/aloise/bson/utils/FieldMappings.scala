package name.aloise.bson.utils

import magnolia.{CaseClass, SealedTrait, Subtype}
import name.aloise.bson.{Configuration, Key}

trait FieldMappings {
  protected def getFieldNameMappings[Typeclass[_], T](caseClass: CaseClass[Typeclass, T])(
    implicit config: Configuration): Map[String, String] = {
    val results =
      caseClass.parameters.map { p =>
        val keyAnnotation = p.annotations.collectFirst {
          case ann: Key => ann
        }
        keyAnnotation match {
          case Some(ann) => p.label -> ann.value
          case None => p.label -> config.fieldNameMapper(p.label)
        }
      }.toMap
    if (results.values.toSet.size != caseClass.parameters.length) {
      throw new IllegalStateException(
        s"Duplicate key detected after applying field name mapper function for case class parameters: $results"
      )
    }
    results
  }

  protected def constructorLookup[Typeclass[_], T](sealedTrait: SealedTrait[Typeclass, T])(implicit config: Configuration): Map[String, Subtype[Typeclass, T]] = {
    val lookup = sealedTrait.subtypes.map(c => config.adtClassNameMapper(c.typeName.short) -> c).toMap
    if (lookup.size != sealedTrait.subtypes.length) {
      throw new IllegalStateException(
        "Duplicate key detected after applying class name mapper function for " +
          "sealed trait child classes"
      )
    }
    lookup
  }
}
