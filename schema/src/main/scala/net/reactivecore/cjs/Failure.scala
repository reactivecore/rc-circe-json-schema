package net.reactivecore.cjs

/** Base trait for Errors */
sealed trait Failure {
  def message: String
}

/** There was an resolving error. */
case class ResolveFailure(val message: String, val cause: Option[Throwable] = None) extends Failure

/** There was an JSON Error. */
case class JsonFailure(val message: String, val cause: io.circe.Error) extends Failure

object JsonFailure {
  def apply(cause: io.circe.Error): JsonFailure = JsonFailure("JSON Failure", cause)
}
