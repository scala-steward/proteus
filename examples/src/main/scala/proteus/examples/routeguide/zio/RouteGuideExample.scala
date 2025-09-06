package proteus.examples.routeguide.zio

import zio.*

import proteus.*
import proteus.examples.routeguide.*

object RouteGuideExample extends ZIOAppDefault {

  val run =
    for {
      _ <- ZIO.log("=== Proteus Route Guide Example ===")
      _ <- ZIO.log(routeGuideService.render(Nil))

      routeNotes <- Ref.make(Map.empty[Point, List[RouteNote]])

      server = RouteGuideServer(8980, routeNotes)
      client = RouteGuideClient("localhost", 8980)

      _ <- ZIO.scoped {
             server.serverResource *>
               ZIO.log("Running Route Guide demo...") *>
               client.runDemo
           }
    } yield ()
}
