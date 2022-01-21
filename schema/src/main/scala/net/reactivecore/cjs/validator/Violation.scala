package net.reactivecore.cjs.validator

import io.circe.Json

/** A Violation which is detected by a Validator */
sealed trait Violation {}

object Violation {
  def apply(json: Json, text: String): SimpleViolation = SimpleViolation(json, text)
}

/** A simple violation */
case class SimpleViolation(json: Json, text: String) extends Violation

/** A violation based upon other violation paths */
case class ComprehensiveViolation(json: Json, text: String, sub: Seq[Seq[Violation]]) extends Violation
