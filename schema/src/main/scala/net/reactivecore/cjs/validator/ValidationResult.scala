package net.reactivecore.cjs.validator

import io.circe.Json

/** The result of a validation. */
case class ValidationResult(
    violations: Seq[Violation]
) {

  /** Returns true if there are no violations. */
  def isSuccess: Boolean = violations.isEmpty

  /** Returns true if there are violations. */
  def isFailure: Boolean = violations.nonEmpty

  /** Merge two validation results */
  def merge(other: ValidationResult): ValidationResult = {
    ValidationResult(violations ++ other.violations)
  }

  override def toString: String = {
    if (isSuccess) {
      "Validation Success"
    } else {
      s"Validation Failure: ${violations.mkString(",")}"
    }
  }
}

object ValidationResult {
  def violation(json: Json, name: String): ValidationResult = {
    ValidationResult(List(Violation(json, name)))
  }

  /** A successful validation */
  def success: ValidationResult = ValidationResult(Nil)
}
