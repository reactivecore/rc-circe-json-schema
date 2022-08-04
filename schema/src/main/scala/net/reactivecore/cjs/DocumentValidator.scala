package net.reactivecore.cjs

import cats.MonadError
import cats.implicits._
import io.circe.Json
import net.reactivecore.cjs.Schema.parse
import net.reactivecore.cjs.resolver.{Downloader, JsonPointer, RefUri, Resolved, Resolver}
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

/** Validator for a full resolved schema document. */
case class DocumentValidator(
    mainId: RefUri,
    roots: Map[RefUri, SingleDocumentValidator]
) {
  private val mainRoot: SingleDocumentValidator = roots(mainId)

  /** Returns the manin schema */
  def mainSchema: Schema = mainRoot.schema

  /** Validate JSON against the Schema. */
  def validate(json: Json): ValidationResult = {
    val (_, violations) = mainRoot.validator.validateStateful(ValidationState.empty, json)(context)
    violations
  }

  private[cjs] object context extends ValidationContext {

    override def resolve(refUri: RefUri): Either[String, Validator] = {
      val rootUri = refUri.copy(
        fragment = None
      )
      roots.get(rootUri) match {
        case None => Left(s"Could not resolve root URI ${rootUri}")
        case Some(root) =>
          refUri.fragment.filter(_.nonEmpty) match {
            case None => Right(root.validator)
            case Some(fragment) =>
              val path = JsonPointer.fromString(fragment)
              path match {
                case Some(path) =>
                  root.findPath(path) match {
                    case None        => Left(s"Could not resolve path: ${path} withing ${rootUri}")
                    case Some(value) => Right(value)
                  }
                case None =>
                  root.fragments.get(fragment) match {
                    case Some(found) => Right(found)
                    case None =>
                      root.dynamicFragments.get(fragment) match {
                        case Some(found) => Right(found)
                        case None        => Left(s"Could not resolve fragment ${fragment} within ${rootUri}")
                      }
                  }
              }
          }
      }
    }

    override def resolveDynamic(
        state: ValidationState,
        refUri: RefUri
    ): Either[String, Validator] = {
      val rootUri = refUri.copy(
        fragment = None
      )
      val root = roots.get(rootUri) match {
        case None       => return Left(s"Could not resolve root of dynamic ref ${refUri}")
        case Some(root) => root
      }
      val fragment = refUri.fragment match {
        case Some(fragment) => fragment
        case None           => return Left(s"Expected fragment for dynamic reference")
      }
      root.dynamicFragments.get(fragment) match {
        case None =>
          // state has no fragment, going back to regular references
          // (See dynamicRef.json acceptance test)
          resolve(refUri)
        case Some(existing) =>
          // Look for oldest overwrite
          val oldestOverwrite = state.stack.reverse.collectFirst {
            case element if element.dynamicAnchors.contains(fragment) => element.dynamicAnchors(fragment)
          }
          oldestOverwrite match {
            case Some(overwritten) =>
              // Had been overwritten
              Right(overwritten)
            case None =>
              // Not overwritten use existing one
              Right(existing)
          }
      }
    }
  }
}

object DocumentValidator {

  /** Build Root Valdiator from Resolved data. */
  def fromResolved(resolved: Resolved): Either[Failure, DocumentValidator] = {
    resolved.roots
      .map { case (id, json) =>
        json.as[Schema].map { schema =>
          id -> SingleDocumentValidator(schema, schema.schemaValidator(id))
        }
      }
      .toVector
      .sequence match {
      case Left(error) => Left(JsonFailure(error))
      case Right(ok) =>
        val asMap = ok.toMap
        Right(DocumentValidator(resolved.main, asMap))
    }
  }
}
