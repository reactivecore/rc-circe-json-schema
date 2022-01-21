package net.reactivecore.cjs.resolver

import cats.MonadError
import cats.implicits._
import io.circe.Json
import io.circe.parser

import java.io.{BufferedReader, InputStreamReader}
import java.net.URL
import scala.language.higherKinds
import scala.util.Try

trait Downloader[F[_]] {

  /** Load the content of the given URL. */
  def loadJson(url: String): F[Json]
}

object Downloader {
  def empty[F[_]](implicit mo: MonadError[F, ResolveError]): Downloader[F] = new Downloader[F] {
    override def loadJson(url: String): F[Json] = mo.raiseError(ResolveError(s"Empty Downloader"))
  }

  def emptySimple: Downloader[SimpleResolveResult] = empty[SimpleResolveResult]

  /**
    * Downloads using Java Url Retrievals.
    * Note: use with trusted sources.
    */
  class JavaUrlDownloader extends Downloader[SimpleResolveResult] {
    override def loadJson(url: String): SimpleResolveResult[Json] = {
      for {
        jUrl <- toUrl(url)
        content <- readUrlToString(jUrl)
        json <- parseJson(url, content)
      } yield json
    }

    private def toUrl(url: String): SimpleResolveResult[URL] = {
      Try(new URL(url)).toEither.left.map { error =>
        ResolveError(s"Invalid URL ${url}: ${error.getMessage}")
      }
    }

    private def readUrlToString(url: URL): SimpleResolveResult[String] = {
      Try {
        val connection = url.openConnection()
        val in = new BufferedReader(new InputStreamReader(connection.getInputStream))
        val buffer = new StringBuilder
        try {
          while ({
            Option(in.readLine()) match {
              case None => false
              case Some(line) =>
                buffer ++= line
                true
            }
          }) ()
        } finally {
          in.close()
        }
        buffer.result()
      }.toEither.left.map { e =>
        ResolveError(s"Could not download ${url.toString}: ${e.getMessage}")
      }
    }

    private def parseJson(url: String, json: String): SimpleResolveResult[Json] = {
      parser.parse(json).left.map { parsingFaiulure =>
        ResolveError(s"Invalid JSON in ${url}: ${parsingFaiulure.message}")
      }
    }
  }
}
