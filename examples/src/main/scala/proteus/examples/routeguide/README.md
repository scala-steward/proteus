# Route Guide Example

A comprehensive gRPC example demonstrating all four types of service methods using Proteus with both ZIO and FS2 backends.

## Running

**ZIO Version:**
```bash
sbt "examples/runMain proteus.examples.routeguide.zio.RouteGuideExample"
```

**FS2 Version:**
```bash
sbt "examples/runMain proteus.examples.routeguide.fs2.RouteGuideExample"
```

## Code Structure

The example is structured with shared definitions at the root level and backend-specific implementations in subfolders:

```
routeguide/
├── RouteGuide.scala      # Shared message definitions and service
├── RouteGuideData.scala  # Shared sample data and utilities
├── zio/                  # ZIO-based implementation
│   ├── RouteGuideClient.scala
│   ├── RouteGuideServer.scala
│   └── RouteGuideExample.scala
└── fs2/                  # FS2/Cats Effect implementation
    ├── RouteGuideClient.scala
    ├── RouteGuideServer.scala
    └── RouteGuideExample.scala
```

**Messages & Service** (`RouteGuide.scala`):
```scala
case class Point(latitude: Int, longitude: Int) derives Schema
case class Rectangle(lo: Point, hi: Point) derives Schema
case class Feature(name: String, location: Point) derives Schema
case class RouteNote(location: Point, message: String) derives Schema
case class RouteSummary(pointCount: Int, featureCount: Int, distance: Int, elapsedTime: Int) derives Schema

val getFeatureRpc = Rpc.unary[Point, Feature]("GetFeature")
val listFeaturesRpc = Rpc.serverStreaming[Rectangle, Feature]("ListFeatures")
val recordRouteRpc = Rpc.clientStreaming[Point, RouteSummary]("RecordRoute")
val routeChatRpc = Rpc.bidiStreaming[RouteNote, RouteNote]("RouteChat")

val routeGuideService = Service("routeguide", "RouteGuide")
  .rpc(getFeatureRpc)
  .rpc(listFeaturesRpc)
  .rpc(recordRouteRpc)
  .rpc(routeChatRpc)
```

**ZIO Server** (`zio/RouteGuideServer.scala`):
```scala
class RouteGuideServer(port: Int, routeNotes: Ref[Map[Point, List[RouteNote]]]) {
  def getFeature(point: Point): UIO[Feature] = // Simple RPC
  def listFeatures(rectangle: Rectangle): ZStream[Any, Nothing, Feature] = // Server streaming  
  def recordRoute(points: ZStream[Any, StatusException, Point]): IO[StatusException, RouteSummary] = // Client streaming
  def routeChat(notes: ZStream[Any, StatusException, RouteNote]): ZStream[Any, StatusException, RouteNote] = // Bidirectional streaming

  val service = ServerService(using ZioServerBackend)
    .rpc(getFeatureRpc, getFeature)
    .rpc(listFeaturesRpc, listFeatures) 
    .rpc(recordRouteRpc, recordRoute)
    .rpc(routeChatRpc, routeChat)
    .build(routeGuideService)

  val start: Task[Unit] = // ZIO-based server start
  val stop: Task[Unit] = // ZIO-based server stop
}
```

**FS2 Server** (`fs2/RouteGuideServer.scala`):
```scala
class RouteGuideServer(port: Int, routeNotes: Ref[IO, Map[Point, List[RouteNote]]]) {
  def getFeature(point: Point): IO[Feature] = // Simple RPC
  def listFeatures(rectangle: Rectangle): Stream[IO, Feature] = // Server streaming  
  def recordRoute(points: Stream[IO, Point]): IO[RouteSummary] = // Client streaming
  def routeChat(notes: Stream[IO, RouteNote]): Stream[IO, RouteNote] = // Bidirectional streaming

  def createService(dispatcher: Dispatcher[IO]) = {
    val backend = Fs2ServerBackend[IO](dispatcher)
    ServerService(using backend)
      .rpc(getFeatureRpc, getFeature)
      .rpc(listFeaturesRpc, listFeatures) 
      .rpc(recordRouteRpc, recordRoute)
      .rpc(routeChatRpc, routeChat)
      .build(routeGuideService)
  }

  val start: IO[Unit] = // Cats Effect-based server start
}
```

## Generated Protobuf

```protobuf
syntax = "proto3";

package routeguide;

service RouteGuide {
    rpc GetFeature (Point) returns (Feature) {}
    rpc ListFeatures (Rectangle) returns (stream Feature) {}
    rpc RecordRoute (stream Point) returns (RouteSummary) {}
    rpc RouteChat (stream RouteNote) returns (stream RouteNote) {}
}

message Point {
    int32 latitude = 1;
    int32 longitude = 2;
}

message Feature {
    string name = 1;
    Point location = 2;
}

message Rectangle {
    Point lo = 1;
    Point hi = 2;
}

message RouteSummary {
    int32 point_count = 1;
    int32 feature_count = 2;
    int32 distance = 3;
    int32 elapsed_time = 4;
}

message RouteNote {
    Point location = 1;
    string message = 2;
}
```

## Features Demonstrated

This example demonstrates all four gRPC service method types following the official Route Guide algorithm:

1. **Simple RPC** (`GetFeature`): Client sends a single point, server returns corresponding feature
2. **Server-side streaming** (`ListFeatures`): Client sends a rectangle, server streams back all features within bounds  
3. **Client-side streaming** (`RecordRoute`): Client streams points, server returns route summary with distance and timing
4. **Bidirectional streaming** (`RouteChat`): Client and server exchange route notes, with server returning existing notes at each location

The demo includes multiple messages to the same location in `routeChat` to demonstrate the bidirectional streaming pattern where later messages receive previously stored notes as responses.
