package proteus

import java.util.concurrent.TimeUnit

import GrpcTestUtils.*
import io.grpc.Metadata
import io.grpc.StatusException
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import scalapb.zio_grpc.{RequestContext, ZChannel}
import zio.*
import zio.test.*

import proteus.client.ZioClientBackend
import proteus.server.{ServerServiceBuilder, ZioServerBackend}

object ZioBackendSpec extends ZIOSpecDefault {

  def processComplexRequestZio(req: ComplexRequest): IO[StatusException, ComplexResponse] =
    ZIO.succeed(processComplexRequest(req))

  def processWithMetadataZio(req: MetadataRequest, ctx: RequestContext): IO[StatusException, MetadataResponse] =
    for {
      requestMetadata <- ctx.metadata.get(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER)).map(_.getOrElse("unknown"))
      _               <- ctx.responseMetadata.put(Metadata.Key.of("server-response", Metadata.ASCII_STRING_MARSHALLER), "processed")
    } yield MetadataResponse(req.message.toUpperCase, requestMetadata, "Server processed with metadata")

  val serverService = ServerServiceBuilder(using ZioServerBackend)
    .rpc(complexRpc, processComplexRequestZio)
    .build(testService)

  val metadataServerService = ServerServiceBuilder(using ZioServerBackend)
    .rpcWithContext(metadataRpc, processWithMetadataZio)
    .build(metadataService)

  def spec = suite("ZioBackendSpec")(
    test("should discover services via gRPC reflection") {
      assertTrue(testReflection(7998, serverService.definition))
    },
    test("should handle complex gRPC request/response with zio backend") {
      val port          = 7999
      val server        = NettyServerBuilder.forPort(port).addService(serverService.definition).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", 7999).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val program = for {
        clientFactory <- clientBackend.client(testService, complexRpc)

        response1 <- clientFactory(sampleRequest)
        response2 <- clientFactory(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low))
        response3 <- clientFactory(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None))
      } yield (response1, response2, response3)

      program
        .flatMap(result => assertTrue(validateComplexResponse(result._1, result._2, result._3)))
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    },
    test("should handle client and server metadata") {
      val port          = 7997
      val server        = NettyServerBuilder.forPort(port).addService(metadataServerService.definition).build().start()
      val channel       = NettyChannelBuilder.forAddress("localhost", 7997).usePlaintext().build()
      val zChannel      = ZChannel(channel, Seq.empty)
      val clientBackend = new ZioClientBackend(zChannel)

      val requestMetadata = new Metadata()
      requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "zio-client-789")
      requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-zio/1.0")

      val program = for {
        clientFactory                <- clientBackend.clientWithMetadata(metadataService, metadataRpc)
        (response, responseMetadata) <- clientFactory(MetadataRequest("hello zio metadata"), requestMetadata)
      } yield (response, responseMetadata)

      program
        .flatMap(result => assertTrue(validateMetadataResponse(result._1, result._2, "zio-client-789", "hello zio metadata")))
        .ensuring(ZIO.attempt {
          server.shutdown().awaitTermination(5, TimeUnit.SECONDS)
          channel.shutdown().awaitTermination(5, TimeUnit.SECONDS)
        }.ignore)
    }
  )
}
