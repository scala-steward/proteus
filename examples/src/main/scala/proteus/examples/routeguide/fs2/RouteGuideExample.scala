package proteus.examples.routeguide.fs2

import cats.effect.{IO, IOApp, Ref}
import cats.effect.std.Dispatcher

import proteus.examples.routeguide.*

object RouteGuideExample extends IOApp.Simple {
  val run: IO[Unit] =
    Dispatcher.parallel[IO].use { implicit dispatcher =>
      for {
        _ <- IO.println("=== Proteus Route Guide Example (FS2) ===")
        _ <- IO.println(routeGuideService.render(Nil))

        routeNotes <- Ref.of[IO, Map[Point, List[RouteNote]]](Map.empty)

        server = RouteGuideServer(8981, routeNotes)
        client = RouteGuideClient("localhost", 8981)

        _ <- server.serverResource.use { _ =>
               IO.println("Running Route Guide demo...") >>
                 client.runDemo
             }
      } yield ()
    }
}
