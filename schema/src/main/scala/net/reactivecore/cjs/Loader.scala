package net.reactivecore.cjs

import cats.MonadError
import cats.implicits._
import io.circe.{Codec, Json}
import io.circe.generic.semiauto
import net.reactivecore.cjs.Vocabulary.VocabularyPart
import net.reactivecore.cjs.resolver.{Downloader, Resolved, Resolver}

import scala.language.{existentials, higherKinds}

/** Combines the different steps to build a SchemaValidator from JSON.
  *
  * Main entry point into handling JSON Schemas.
  *
  * @param downloader the downloader to download referenced schemas or meta schemas
  * @param defaultVocabulary the default vocabulary to use if none is given
  */
case class Loader[F[_]](
    downloader: Downloader[F],
    defaultVocabulary: Vocabulary = Vocabularies.vocabulary2020
)(
    implicit monad: MonadError[F, Failure]
) {

  /** Load and resolve a JSON Schema from an URL and build a DocumentValidator. */
  def fromUrl(url: String): F[DocumentValidator] = {
    downloader.loadJson(url).flatMap(fromJson)
  }

  /** Load and resolve a JSON Schema from an JSON Source and build a DocumentValidator. */
  def fromJson(json: String): F[DocumentValidator] = {
    monad.fromEither(io.circe.parser.parse(json).left.map(e => JsonFailure(e))).flatMap(fromJson)
  }

  /** Load and resolve a JSON */
  def fromJson(json: Json): F[DocumentValidator] = {
    for {
      vocabulary <- determineVocabulary(json)
      appliedVocabulary = vocabulary.filter(json)
      resolver = new Resolver(downloader, Some(vocabulary.filter))
      resolved <- resolver.resolve(appliedVocabulary)
      resolvedAfterVocabulary = filterWithVocabulary(resolved, vocabulary)
      documentValidator <- monad.fromEither(DocumentValidator.fromResolved(resolvedAfterVocabulary))
    } yield {
      documentValidator
    }
  }

  /** The part of a Json schema which references the schema */
  private case class SchemaReference(
      `$schema`: String
  )

  private implicit val schemaReferenceCodec: Codec.AsObject[SchemaReference] = semiauto.deriveCodec

  private def determineVocabulary(json: Json): F[Vocabulary] = {
    json.as[SchemaReference] match {
      case Left(_)                => monad.pure(defaultVocabulary)
      case Right(schemaReference) =>
        // Looking for known ones
        Vocabularies.vocabularies.find(_.schemaId == schemaReference.`$schema`) match {
          case Some(known) => monad.pure(known)
          case None =>
            for {
              metaSchemaJson <- downloader.loadJson(schemaReference.`$schema`)
              parsedMetaSchemaJson <- monad.fromEither(metaSchemaJson.as[MetaSchema].left.map(JsonFailure(_)))
              vocabulary <- buildVocabulary(schemaReference.`$schema`, parsedMetaSchemaJson)
            } yield vocabulary
        }
    }
  }

  private def buildVocabulary(id: String, metaSchema: MetaSchema): F[Vocabulary] = {
    metaSchema.`$vocabulary` match {
      case None => monad.pure(defaultVocabulary)
      case Some(pairs) =>
        val maybeParts: F[Vector[Option[VocabularyPart]]] = pairs
          .map { case (partName, required) =>
            Vocabularies.knownParts.get(partName) match {
              case None if required =>
                monad.raiseError(
                  ResolveFailure(s"Unsupported required vocabulary part: ${partName} in meta schema ${id}")
                ): F[Option[VocabularyPart]]
              case None       => monad.pure(None): F[Option[VocabularyPart]]
              case Some(part) => monad.pure(Some(part)): F[Option[VocabularyPart]]
            }
          }
          .toVector
          .sequence

        maybeParts
          .map(_.flatten)
          .map { parts =>
            Vocabulary(
              id,
              parts
            )
          }
    }
  }

  /** Filter resolved JSON using a vocabulary */
  private def filterWithVocabulary(resolved: Resolved, vocabulary: Vocabulary): Resolved = {
    resolved.copy(
      roots = resolved.roots.mapValues(vocabulary.filter).view.toMap
    )
  }
}

object Loader {

  /** Returns a Loader with disabled Downloading. */
  def empty: Loader[Result] = Loader(Downloader.emptySimple)
}
