package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.validator.Validator.Compound

case class NotValidator(validator: Validator) extends Compound(Vector(validator)) {
  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    if (validator.validateStateful(state.withoutEvaluated, json)(context)._2.isSuccess) {
      (state, ValidationResult.violation(json, "Not"))
    } else {
      (state, ValidationResult.success)
    }
  }
}

case class AnyOfValidator(validators: Vector[Validator]) extends Compound(validators) {
  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {

    // Interestingly we must match all of them in order to get the state right
    val subResults = validators.map { validator =>
      validator.validateStateful(state, json)
    }

    val successfulStates = subResults.collect {
      case x if x._2.isSuccess => x._1
    }
    if (successfulStates.isEmpty) {
      val errors = subResults.map(_._2)
      state -> ValidationResult(Seq(ComprehensiveViolation(json, "Or", errors.map(_.violations))))
    } else {
      val resultingState = successfulStates.reduce { (left, right) =>
        ValidationState(
          evaluatedProperties = left.evaluatedProperties ++ right.evaluatedProperties,
          evaluatedIndices = left.evaluatedIndices ++ right.evaluatedIndices,
          stack = state.stack // ??
        )
      }
      resultingState -> ValidationResult.success
    }
  }
}

case class OneOfValidator(validators: Vector[Validator]) extends Compound(validators) {
  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val validations = validators
      .collect {
        case validator if validator.validateStateful(state, json)(context)._2.isSuccess =>
          validator.validateStateful(state, json)(context)
      }
    validations.size match {
      case 1 => validations.head
      case n =>
        state -> ValidationResult.violation(json, s"Expected one of ${validators.size} to be successful, but got ${n}")
    }
  }
}

case class AllOfValidator(validations: Vector[Validator]) extends Compound(validations) {
  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val results = validations.map { validator =>
      validator.validateStateful(state, json)
    }
    results.collectFirst {
      case (_, violations) if violations.isFailure => violations
    } match {
      case None =>
        val resultingState = results.foldLeft(state) { case (current, (nextState, _)) =>
          current.mergeEvaluated(nextState)
        }
        resultingState -> ValidationResult.success
      case Some(errors) =>
        state -> errors
    }
  }

  override def touch(state: ValidationState): ValidationState = {
    validations.foldLeft(state) { case (state, child) =>
      child.touch(state)
    }
  }

  override def precedence: Int = -1
}

case class IfThenElseValidator(
    ifClause: Validator,
    thenClause: Option[Validator],
    elseClause: Option[Validator]
) extends Validator {
  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val (ifState, ifValidated) = ifClause.validateStateful(state, json)
    if (ifValidated.isSuccess) {
      thenClause.map(_.validateStateful(ifState, json)).getOrElse(ifState -> ValidationResult.success)
    } else {
      elseClause.map(_.validateStateful(ifState, json)).getOrElse(ifState -> ValidationResult.success)
    }
  }

  override def precedence: Int = {
    // Must be before Definitions
    -2
  }
}
