package net.reactivecore.cjs

import cats.MonadError
import cats.implicits._
import io.circe.Json
import net.reactivecore.cjs.resolver.{Downloader, Resolver}

import scala.language.higherKinds

/** Combines the different steps to build a SchemaValidator from JSON */
class Loader[F[_]](downloader: Downloader[F])(
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
      documentValidator <- monad.fromEither(DocumentValidator.build(resolved))
    } yield {
      documentValidator
    }
  }
}
