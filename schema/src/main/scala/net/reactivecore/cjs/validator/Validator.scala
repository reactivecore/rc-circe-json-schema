package net.reactivecore.cjs.validator

import io.circe.Json
import net.reactivecore.cjs.SchemaOrigin
import net.reactivecore.cjs.resolver.{JsonPointer, RefUri}

import scala.collection.mutable

/** Validates JSON.
  * This are the core elements which form a validation tree.
  */
trait Validator {

  /** Touches the validator, possible state modifying (e.g. adding dynamic anchors) */
  def touch(state: ValidationState): ValidationState = state

  /** Validate JSON, returns violations. */
  def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult)

  /** Validate, but skip evaluated */
  def validateWithoutEvaluated(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): ValidationResult = {
    validateStateful(state.withoutEvaluated, json)._2
  }

  /**
    * Precedence of this operation (for stateful validators), when using Sequence
    */
  def precedence: Int = 0

  /** Children Validators */
  def children: Vector[Validator] = Vector.empty

  /** Depth first foreach. */
  def wideForeach(f: Validator => Unit): Unit = {
    val wait = mutable.Queue[Validator]()
    wait += this
    while (wait.nonEmpty) {
      val first = wait.dequeue()
      f(first)
      first.children.foreach { child =>
        wait += child
      }
    }
  }
}

/** A Validator which is is the base of a single schema. */
trait SchemaValidator extends Validator {

  def origin: SchemaOrigin

  /** The path inside the schema. */
  final def path: JsonPointer = origin.path

  /** An Explicit fragment of this Validator */
  def fragment: Option[String] = None

  /** Dynamic Fragment (dynamic anchor) of this validator */
  def dynamicFragment: Option[String] = None

  /** This schema defines it's own id. */
  def idOverride: Option[RefUri] = None
}

trait ContextFreeValidator extends Validator {
  def validate(state: ValidationState, json: Json): (ValidationState, ValidationResult)

  override def validateStateful(state: ValidationState, json: Json)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    validate(state, json)
  }
}

/** A Simple context free validator which just emits an error message if some JSON is not valid. */
abstract class SimpleContextFreeValidator(name: String) extends ContextFreeValidator {

  def isValid(json: Json): Boolean

  override def validate(state: ValidationState, json: Json): (ValidationState, ValidationResult) = {
    if (!isValid(json)) {
      state -> ValidationResult.violation(json, name)
    } else {
      state -> ValidationResult.success
    }
  }
}

object Validator {

  def simple(name: String)(ok: Json => Boolean): ContextFreeValidator = (state: ValidationState, json: Json) => {
    if (ok(json)) {
      (state, ValidationResult.success)
    } else {
      (state, ValidationResult.violation(json, name))
    }
  }

  def contextFree(f: Json => ValidationResult): Validator = new Validator {
    override def validateStateful(state: ValidationState, json: Json)(
        implicit context: ValidationContext
    ): (ValidationState, ValidationResult) = {
      state -> f(json)
    }
  }

  /** A Success validator. */
  case object Success extends ContextFreeValidator {
    override def validate(state: ValidationState, json: Json): (ValidationState, ValidationResult) =
      (state, ValidationResult.success)
  }
  val success: ContextFreeValidator = Success

  /** Sequence of validators
    * Note: in contrast to allOf this parallel validators can be combined.
    */
  private case class Sequence(ordered: Vector[Validator]) extends Validator {

    override def touch(state: ValidationState): ValidationState = {
      ordered.foldLeft(state) { case (currentState, next) =>
        next.touch(currentState)
      }
    }

    override def validateStateful(state: ValidationState, json: Json)(
        implicit context: ValidationContext
    ): (ValidationState, ValidationResult) = {
      val it = ordered.iterator
      var currentState = state
      while (it.hasNext) {
        val validator = it.next()
        val (nextState, violations) = validator.validateStateful(currentState, json)
        if (violations.isFailure) {
          return (state, violations)
        }
        currentState = nextState
      }
      (currentState, ValidationResult.success)
    }

    override def children: Vector[Validator] = {
      ordered
    }
  }

  /** A Compound validator containg other validators. */
  abstract class Compound(override val children: Vector[Validator]) extends Validator {}

  /** Chains validators after each other. */
  def sequence(validators: Validator*): Validator = {
    val flat = validators.flatMap {
      case Sequence(validators) => validators
      case s if s eq success    => Nil
      case other                => Seq(other)
    }
    flat.size match {
      case 0 => Validator.success
      case 1 => flat.head
      case _ => Sequence(flat.sortBy(_.precedence).toVector)
    }
  }

  /** Convenience method. */
  def sequenceOfOpts(validators: Option[Validator]*): Validator = {
    val nonEmpty = validators.flatten
    sequence(nonEmpty: _*)
  }
}
