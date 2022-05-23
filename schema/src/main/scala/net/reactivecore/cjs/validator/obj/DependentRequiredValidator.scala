package net.reactivecore.cjs.validator.obj
import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.util.VectorMap
import net.reactivecore.cjs.validator.{ValidationProvider, ValidationResult, Violation}

case class DependentRequiredValidator(dependentRequired: VectorMap[String, Vector[String]]) extends StatelessValidator {
  override def validate(obj: JsonObject): ValidationResult = {
    val violations = for {
      (key, required) <- dependentRequired.toVector
      if obj.contains(key)
      missing = required.filterNot(obj.contains)
      if missing.nonEmpty
    } yield {
      Violation(Json.fromJsonObject(obj), s"dependentRequired for key ${key} requires ${missing}")
    }
    ValidationResult(violations)
  }
}
