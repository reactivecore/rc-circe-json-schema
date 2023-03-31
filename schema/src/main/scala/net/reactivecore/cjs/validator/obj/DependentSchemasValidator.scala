package net.reactivecore.cjs.validator.obj
import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.Schema
import net.reactivecore.cjs.util.VectorMap
import net.reactivecore.cjs.validator._
import net.reactivecore.cjs.restriction.ValidatingField

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

object DependentSchemasValidator {
  implicit val provider: ValidationProvider[ValidatingField[VectorMap[String, Schema], DependentSchemasValidator]] = ValidationProvider.forField[VectorMap[String, Schema], DependentSchemasValidator] {
    (origin, dependentSchemas) =>
      val withSchemas = dependentSchemas.mapValues(_.validator(origin)).view.toMap
      DependentSchemasValidator(withSchemas)
  }
}
