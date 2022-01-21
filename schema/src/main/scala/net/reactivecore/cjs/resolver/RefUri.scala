package net.reactivecore.cjs.resolver

import io.circe.{Decoder, Encoder}

import java.net.{URI, URISyntaxException}

/** Wraps Javas' URI but feels more scala like */
case class RefUri(
    scheme: Option[String] = None,
    userInfo: Option[String] = None,
    host: Option[String] = None,
    port: Option[Int] = None,
    path: Option[String] = None,
    query: Option[String] = None,
    fragment: Option[String] = None
) {

  /** Converts to Java URI */
  def toUri: URI = {
    new URI(
      scheme.orNull,
      userInfo.orNull,
      host.orNull,
      port match {
        case None    => -1
        case Some(n) => n
      },
      path.orNull,
      query.orNull,
      fragment.orNull
    )
  }

  /** Resolve an uri. */
  def resolve(other: RefUri): RefUri = {
    RefUri.fromUri(
      toUri.resolve(other.toUri)
    )
  }

  /** Resolve if other is set (convenience function) */
  def maybeResolve(other: Option[RefUri]): RefUri = {
    other.map { resolve }.getOrElse(this)
  }

  /** Append path like fragments. If there is currently not a path like one, it will be replaced. */
  def appendPathFragment(path: String): RefUri = {
    val parts = path.split('/').toSeq
    parts.foldLeft(this) { case (current, next) =>
      current.appendSinglePathFragment(next)
    }
  }

  private def appendSinglePathFragment(fragment: String): RefUri = {
    this.fragment match {
      case Some(existing) if existing.startsWith("/") =>
        copy(fragment = Some(existing + "/" + fragment))
      case otherwise =>
        copy(fragment = Some("/" + fragment))
    }
  }

  /** Converts to canonical string */
  override def toString: String = {
    toUri.toString
  }
}

object RefUri {

  def fromString(s: String): Either[String, RefUri] = {
    try {
      Right(forceString(s))
    } catch {
      case s: URISyntaxException => Left(Option(s.getMessage).getOrElse(s"Unknown error"))
    }
  }

  def forceString(s: String): RefUri = {
    fromUri(new URI(s))
  }

  def fromUri(uri: URI): RefUri = {
    RefUri(
      scheme = Option(uri.getScheme),
      userInfo = Option(uri.getUserInfo),
      host = Option(uri.getHost),
      port = Some(uri.getPort).filter(_ != -1),
      path = Option(uri.getPath).filter(_.nonEmpty),
      query = Option(uri.getQuery),
      fragment = Option(uri.getFragment)
    )
  }

  implicit val decoder: Decoder[RefUri] = Decoder.decodeString.emap {
    RefUri.fromString
  }

  implicit val encoder: Encoder[RefUri] = Encoder.encodeString.contramap(_.toString)
}
