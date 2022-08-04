package net.reactivecore.cjs.resolver

import cats.implicits._
import cats.{Monad, MonadError}
import io.circe.{Json, JsonObject}
import net.reactivecore.cjs.{Failure, ResolveFailure}

import scala.language.higherKinds

case class ResolvingState(
    schemas: Map[RefUri, Json],
    changes: Int
) {

  /** Add another root. */
  def withSchemaRoot(refUri: RefUri, json: Json): ResolvingState = {
    copy(schemas = schemas + (refUri -> json))
  }

  /** Applies a function f on each element of iterable, changing the state on each step */
  def effectFold[T, R, F[_]](
      iterable: Iterable[T]
  )(f: (T, ResolvingState) => F[(R, ResolvingState)])(implicit monad: Monad[F]): F[(List[R], ResolvingState)] = {
    // TODO: I am pretty sure Cat has something better for this task. (maybe State, or Eval)
    def continue(
        current: ResolvingState,
        pending: List[T],
        resultBuffer: List[R]
    ): F[(List[R], ResolvingState)] = {
      pending match {
        case Nil =>
          monad.pure((resultBuffer, current))
        case first :: rest =>
          f(first, current).flatMap { case (result, nextState) =>
            continue(nextState, rest, result :: resultBuffer)
          }
      }
    }

    continue(this, iterable.toList, Nil).map { case (results, state) =>
      results.reverse -> state
    }
  }
}

/** Resolves references within JSON.
  * @param downloader the downloader
  * @param jsonFilter the filter to be applied after downloading a Schema (for Vocabularies)
  */
class Resolver[F[_]](downloader: Downloader[F], jsonFilter: Option[Json => Json] = None)(
    implicit monad: MonadError[F, Failure]
) {

  def resolve(json: Json): F[Resolved] = {
    val initialId = withResolveable(json) { r =>
      r.`$id`
    }.getOrElse(RefUri())
    val initialState = buildInitialState(initialId, json)
    resolveRoots(0, initialState, json, initialId).map { finalState =>
      Resolved(
        initialId,
        finalState.schemas
      )
    }
  }

  /** Build initial state with all contained sub schemas. */
  private def buildInitialState(initialId: RefUri, json: Json): ResolvingState = {
    def recursiveCollectSubSchemas(base: RefUri, json: Json): Iterable[(RefUri, Json)] = {
      withResolveable(json) { resolvable =>
        val maybeId = resolvable.`$id`.map(base.resolve)
        val newBaseId = maybeId.getOrElse(base)
        val thisItem = maybeId.map { id =>
          id -> json
        }
        val childrenItems = jsonChildren(json, Resolver.BlacklistParentKey).flatMap { childJson =>
          recursiveCollectSubSchemas(newBaseId, childJson)
        }
        thisItem ++ childrenItems
      }
    }
    val schemas = recursiveCollectSubSchemas(initialId, json)
    ResolvingState(
      schemas.toMap + (initialId -> json),
      changes = 0
    )
  }

  private def withResolveable[T](json: Json)(f: ResolveablePiece => T): T = {
    val piece = json.as[ResolveablePiece].getOrElse(ResolveablePiece())
    f(piece)
  }

  /** Returns an iterator to all children of a JSON element.
    * @param keyblacklist if given, this keys are ignored
    */
  private def jsonChildren(json: Json, keyblacklist: Seq[String] = Nil): Iterable[Json] = {
    json.arrayOrObject(
      or = Iterable.empty,
      jsonArray = a => a,
      jsonObject = {
        if (keyblacklist.isEmpty) { o =>
          o.values
        } else { o =>
          o.toIterable.collect {
            case (key, value) if !keyblacklist.contains(key) => value
          }
        }
      }
    )
  }

  private def resolveRoots(depth: Int, state: ResolvingState, json: Json, parentId: RefUri): F[ResolvingState] = {
    withResolveable(json) { resolvable =>
      val subId = resolvable.`$id`.map { id =>
        parentId.resolve(id)
      }

      val position = subId.getOrElse(parentId)

      val reference = resolvable.`$ref`.map { ref =>
        position.resolve(ref)
      }

      val absoluteId = subId.map(_.copy(fragment = None))
      val referenceId = reference.map(_.copy(fragment = None))

      val absoluteReferencedState = absoluteId
        .map { id =>
          if (state.schemas.contains(id)) {
            state
          } else {
            state.withSchemaRoot(id, json)
          }
        }
        .getOrElse(state)

      referenceId
        .map { id => getRaw(depth + 1, id, absoluteReferencedState) }
        .getOrElse(monad.pure(absoluteReferencedState))
        .flatMap { referencedIdState =>
          referencedIdState.effectFold(jsonChildren(json, Resolver.BlacklistParentKey)) { (child, state) =>
            resolveRoots(depth + 1, state, child, position).map { newState =>
              () -> newState
            }
          }
        }
        .map { _._2 }
    }
  }

  private def getRaw(depth: Int, uri: RefUri, state: ResolvingState): F[ResolvingState] = {
    if (state.schemas.contains(uri)) {
      monad.pure(state)
    } else {
      for {
        downloaded <- downloader.loadJson(uri.toString)
        filtered = jsonFilter.fold(downloaded)(_.apply(downloaded))
        updatedState = state.withSchemaRoot(uri, filtered)
        reflected <- resolveRoots(depth + 1, updatedState, filtered, uri)
      } yield reflected
    }
  }
}

object Resolver {

  /** Parent keys which are blacklisted from being used for $id or $ref */
  val BlacklistParentKey = Seq("const", "enum")
}
