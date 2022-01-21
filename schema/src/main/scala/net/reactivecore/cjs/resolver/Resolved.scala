package net.reactivecore.cjs.resolver

import io.circe.Json

/** The result of resolved JSON data */
case class Resolved(
    main: RefUri,
    roots: Map[RefUri, Json]
)
