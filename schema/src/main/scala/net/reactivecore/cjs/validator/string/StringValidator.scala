package net.reactivecore.cjs.validator.string

import io.circe.Json
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator}

import java.util.regex.Pattern

abstract class StringValidator(name: String) extends Validator {
  def isValid(s: String): Boolean

  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    if (json.asString.forall(isValid)) {
      state -> ValidationResult.success
    } else {
      state -> ValidationResult.violation(json, name)
    }
  }
}

object StringValidator {
  case class PatternValidator(pattern: String) extends StringValidator("pattern") {
    val compiled = Pattern.compile(pattern)

    override def isValid(s: String): Boolean = {
      // Note: this is not secure on Java < 9
      compiled.matcher(s).find()
    }
  }

  case class MinLengthValidator(minLength: Int) extends StringValidator("minLength") {
    override def isValid(s: String): Boolean = {
      codePointLength(s) >= minLength
    }
  }

  case class MaxLengthValidator(maxLength: Int) extends StringValidator("maxLength") {
    override def isValid(s: String): Boolean = {
      codePointLength(s) <= maxLength
    }
  }

  /** We need the number of codepoints for string length comparisons. */
  private def codePointLength(s: String): Int = {
    s.codePointCount(0, s.length)
  }
}
