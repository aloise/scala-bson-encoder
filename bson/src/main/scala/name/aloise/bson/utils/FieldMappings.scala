package name.aloise.bson.utils

import magnolia.CaseClass
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
}
