package net.reactivecore.cjs

import cats.MonadError
import cats.implicits._
import io.circe.Json
import net.reactivecore.cjs.resolver.{Downloader, Resolved, Resolver}

import scala.language.higherKinds

/** Combines the different steps to build a SchemaValidator from JSON */
class Loader[F[_]](
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
    val resolver = new Resolver(downloader)
    for {
      resolved <- resolver.resolve(json)
      vocabulary <- monad.fromEither(determineVocabulary(resolved))
      resolvedAfterVocabulary = filterWithVocabulary(resolved, vocabulary)
      documentValidator <- monad.fromEither(DocumentValidator.build(resolvedAfterVocabulary))
    } yield {
      documentValidator
    }
  }

  private def determineVocabulary(resolved: Resolved): Result[Vocabulary] = {
    // TODO TODO TODO!
    Right(defaultVocabulary)
  }

  /** Filter resolved JSON using a vocabulary */
  private def filterWithVocabulary(resolved: Resolved, vocabulary: Vocabulary): Resolved = {
    resolved.copy(
      roots = resolved.roots.mapValues(vocabulary.filter).view.toMap
    )
  }
}
