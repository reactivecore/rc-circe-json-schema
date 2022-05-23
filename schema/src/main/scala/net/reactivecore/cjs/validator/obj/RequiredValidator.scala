package net.reactivecore.cjs.validator.obj
import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.validator.{ValidationProvider, ValidationResult, Violation}

case class RequiredValidator(fields: Seq[String]) extends StatelessValidator {
  override def validate(json: JsonObject): ValidationResult = {
    val missing = fields.diff(json.keys.toSeq)
    if (missing.nonEmpty) {
      ValidationResult.violation(Json.fromJsonObject(json), s"Missing required ${missing.mkString(",")}")
    } else {
      ValidationResult.success
    }
  }
}
