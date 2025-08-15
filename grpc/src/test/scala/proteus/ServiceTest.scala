package proteus

import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import io.grpc.protobuf.services.ProtoReflectionServiceV1
import zio.*
import zio.blocks.schema.Schema

import proteus.client.DirectClientBackend
import proteus.server.*

case class HelloRequest(name: String) derives Schema
case class HelloResponse(message: String) derives Schema
case class TestRequest(name: String) derives Schema
case class TestResponse(message: String) derives Schema

enum MyEnum derives Schema {
  case A
  case B
  case C
}

enum MyOneOf {
  case Case1(name: String)
  case Case2(age: Int)
}

case class ComplexRequest(name: String, age: Int, list: List[Int], map: Map[String, Int], e: MyEnum, oneOf: MyOneOf) derives Schema
case class ComplexResponse(name: String, age: Int, list: List[Int], map: Map[String, Int], e: MyEnum, oneOf: MyOneOf) derives Schema
case class ComplexRequest2(name: String, age: Int, list: List[Int], map: Map[String, Int], e: MyEnum, oneOf: MyOneOf) derives Schema
case class ComplexResponse2(name: String, age: Int, list: List[Int], map: Map[String, Int], e: MyEnum, oneOf: MyOneOf) derives Schema
case class ComplexRequest3(name: String, age: Int, list: List[Int], map: Map[String, Int], e: MyEnum, oneOf: MyOneOf) derives Schema
case class ComplexResponse3(name: String, age: Int, list: List[Int], map: Map[String, Int], e: MyEnum, oneOf: MyOneOf) derives Schema

val commonEntities = Import("common_entities.proto")
val entities       = Import("entities.proto")

val helloRpc     = Rpc.unary[HelloRequest, HelloResponse]("Hello")
val helloService = Service("HelloService").rpc(helloRpc)

val complexRpc     = Rpc.unary[ComplexRequest, ComplexResponse]("Complex")
val complexService = Service("ComplexService").imports(commonEntities).imports(entities).rpc(complexRpc)

val complexRpc2     = Rpc.unary[ComplexRequest2, ComplexResponse2]("Complex2")
val complexService2 = Service("ComplexService2").imports(commonEntities).imports(entities).rpc(complexRpc2)

val complexRpc3     = Rpc.unary[ComplexRequest3, ComplexResponse3]("Complex3")
val complexService3 = Service("ComplexService3").imports(commonEntities).imports(entities).rpc(complexRpc3)

val commonEntitiesDep = commonEntities.toDependency.add[MyEnum]
val entitiesDep       = entities.toDependency(helloService, complexService, complexService2, complexService3).dependsOn(commonEntitiesDep)

val helloServerService = ServerServiceBuilder(using DirectServerBackend)
  .rpc(helloRpc, req => HelloResponse(s"Hello, ${req.name}"))
  .build(helloService)

val complexServerService =
  ServerServiceBuilder(using DirectServerBackend)
    .dependsOn(commonEntitiesDep)
    .dependsOn(entitiesDep)
    .rpc(complexRpc, req => ComplexResponse(req.name, req.age, req.list, req.map, req.e, req.oneOf))
    .build(complexService)

val complexServerService2 =
  ServerServiceBuilder(using DirectServerBackend)
    .dependsOn(commonEntitiesDep)
    .dependsOn(entitiesDep)
    .rpc(complexRpc2, req => ComplexResponse2(req.name, req.age, req.list, req.map, req.e, req.oneOf))
    .build(complexService2)

val complexServerService3 =
  ServerServiceBuilder(using DirectServerBackend)
    .dependsOn(commonEntitiesDep)
    .dependsOn(entitiesDep)
    .rpc(complexRpc3, req => ComplexResponse3(req.name, req.age, req.list, req.map, req.e, req.oneOf))
    .build(complexService3)

val options = List(
  ProtoIR.TopLevelOption("java_package", "com.devsisters.ck.services.game.protobuf"),
  ProtoIR.TopLevelOption("csharp_namespace", "Services.Game.Protobuf")
)

@main
def mainComplexService: Unit = {
  val port = 8081

  println(commonEntitiesDep.render(Some("ck.game"), options))
  println(entitiesDep.render(Some("ck.game"), options))
  println(helloService.render(Some("ck.game"), options))

  val builder = NettyServerBuilder.forPort(port).addService(ProtoReflectionServiceV1.newInstance())
  builder.addService(helloServerService.definition)
  builder.addService(complexServerService.definition)
  builder.addService(complexServerService2.definition)
  builder.addService(complexServerService3.definition)
  val server  = builder.build().start()

  println(s"Server started on port $port")

  val channel     = NettyChannelBuilder.forAddress("localhost", port).usePlaintext().build()
  val helloClient = DirectClientBackend(channel).client(helloService, helloRpc)
  println(helloClient(HelloRequest("World")))

  server.awaitTermination()
}
