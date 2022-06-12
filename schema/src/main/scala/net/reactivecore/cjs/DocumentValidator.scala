package net.reactivecore.cjs

import cats.MonadError
import cats.implicits._
import io.circe.Json
import net.reactivecore.cjs.Schema.parse
import net.reactivecore.cjs.resolver.{Downloader, JsonPointer, RefUri, ResolveError, Resolved, Resolver}
import net.reactivecore.cjs.validator.{ValidationContext, ValidationResult, ValidationState, Validator, Violation}

/** Validator for a full resolved schema document. */
case class DocumentValidator(
    mainId: RefUri,
    roots: Map[RefUri, SingleDocumentValidator]
) {
  private val mainRoot = roots(mainId)

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

  /** Build a document validator from resolved state. */
  def build(resolved: Resolved): Either[String, DocumentValidator] = {
    resolved.roots
      .map { case (id, json) =>
        for {
          schema <- json.as[Schema].left.map { failure =>
            s"Could not decode schema: ${failure}"
          }
          maybeMeta <- resolveMetaSchema(schema, resolved)
        } yield {
          id -> SingleDocumentValidator(schema, schema.schemaValidator(id, maybeMeta))
        }
      }
      .toVector
      .sequence match {
      case Left(error) => Left(s"Decoding Error: ${error}")
      case Right(ok) =>
        val asMap = ok.toMap
        Right(DocumentValidator(resolved.main, asMap))
    }
  }

  /** Find the meta schema, if given. */
  private def resolveMetaSchema(schema: Schema, resolved: Resolved): Either[String, Option[ObjectSchema]] = {
    schema match {
      case b: BooleanSchema => Right(None)
      case o: ObjectSchema =>
        o.location.`$schema`.map { metaSchemaUri =>
          resolved.roots.get(metaSchemaUri) match {
            case None       => Left(s"Could not resolve meta schema ${metaSchemaUri}")
            case Some(json) => json.as[ObjectSchema].left.map { _ => "Could not decode meta schema" }
          }
        }.sequence
    }
  }

  /** Convenience method for Parsing and Resolving a Schema. */
  def parseAndResolveJson[F[_]](
      schemaJson: String,
      downloader: Downloader[F]
  )(implicit applicativeError: MonadError[F, ResolveError]): F[DocumentValidator] = {
    parse(schemaJson) match {
      case Left(err)     => applicativeError.raiseError(ResolveError(s"Parsing error: ${err.getMessage}"))
      case Right(schema) => schema.resolve(downloader)
    }
  }

  /** Convenience Method for loading and parsing a Schema from URL. */
  def parseAndResolveFromUrl[F[_]](
      url: String,
      downloader: Downloader[F]
  )(implicit applicativeError: MonadError[F, ResolveError]): F[DocumentValidator] = {
    val resolver = new Resolver(downloader)
    for {
      base <- downloader.loadJson(url)
      resolved <- resolver.resolve(base)
      validator <- DocumentValidator
        .build(resolved)
        .fold(
          err => applicativeError.raiseError(ResolveError(err)),
          ok => applicativeError.pure(ok)
        )
    } yield validator
  }
}
