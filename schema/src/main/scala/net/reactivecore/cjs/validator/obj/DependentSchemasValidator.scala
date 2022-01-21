package net.reactivecore.cjs.validator.obj
import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.validator._

case class DependentSchemasValidator(withSchemas: Map[String, Validator]) extends ObjectValidator {
  override def validateStatefulObject(state: ValidationState, json: JsonObject)(
      implicit context: ValidationContext
  ): (ValidationState, ValidationResult) = {
    val applicable = withSchemas.filterKeys(json.contains)
    // According to spec the same as the AllOfValidator
    val combined = AllOfValidator(applicable.values.toVector)
    combined.validateStateful(state, Json.fromJsonObject(json))
  }
}
