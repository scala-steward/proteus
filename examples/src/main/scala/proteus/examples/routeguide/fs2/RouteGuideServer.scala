package proteus.examples.routeguide.fs2

import java.util.concurrent.TimeUnit

import cats.effect.{IO, Ref}
import cats.effect.std.Dispatcher
import fs2.Stream
import io.grpc.ServerBuilder

import proteus.examples.routeguide.*
import proteus.server.{Fs2ServerBackend, ServerService}

class RouteGuideServer(port: Int, routeNotes: Ref[IO, Map[Point, List[RouteNote]]])(using dispatcher: Dispatcher[IO]) {
  def getFeature(point: Point): IO[Feature] =
    IO.println(s"Server: GetFeature(${point.latitude}, ${point.longitude})")
      .as(RouteGuideData.findFeature(point))

  def listFeatures(rectangle: Rectangle): Stream[IO, Feature] =
    Stream.eval(IO.println("Server: ListFeatures")) >>
      Stream.emits(RouteGuideData.findFeaturesInRectangle(rectangle))

  def recordRoute(points: Stream[IO, Point]): IO[RouteSummary] =
    for {
      _                        <- IO.println("Server: RecordRoute")
      startTime                <- IO.realTime.map(_.toMillis)
      result                   <- points
                                    .fold((0, 0, Option.empty[Point])) { case ((pointCount, distance, lastPoint), point) =>
                                      val newDistance = lastPoint.map(RouteGuideData.calcDistance(_, point)).getOrElse(0) + distance
                                      (pointCount + 1, newDistance, Some(point))
                                    }
                                    .compile
                                    .lastOrError
      endTime                  <- IO.realTime.map(_.toMillis)
      (pointCount, distance, _) = result
      elapsedTime               = ((endTime - startTime) / 1000).toInt
    } yield RouteSummary(pointCount, 0, distance, elapsedTime)

  def routeChat(notes: Stream[IO, RouteNote]): Stream[IO, RouteNote] =
    notes.evalMap { note =>
      for {
        oldMap       <- routeNotes.getAndUpdate(map => map + (note.location -> (note :: map.getOrElse(note.location, List.empty))))
        existingNotes = oldMap.getOrElse(note.location, List.empty)
      } yield Stream.emits(existingNotes)
    }.flatten

  val backend = Fs2ServerBackend[IO](dispatcher)

  val service = ServerService(using backend)
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures)
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  val server = ServerBuilder.forPort(port).addService(service).build()

  val start: IO[Unit] =
    IO.delay {
      server.start()
      sys.addShutdownHook(unsafeStop())
    } >> IO.println(s"Server started on port $port")

  val stop: IO[Unit] =
    IO.delay {
      unsafeStop()
    }

  def unsafeStop(): Unit =
    server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
}
