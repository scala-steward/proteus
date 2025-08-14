package proteus

import java.util.concurrent.TimeUnit

import cats.effect.IO
import cats.effect.std.Dispatcher
import cats.effect.unsafe.implicits.global

import GrpcTestUtils.*
import io.grpc.Metadata
import io.grpc.netty.{NettyChannelBuilder, NettyServerBuilder}
import zio.test.*

import proteus.client.Fs2ClientBackend
import proteus.server.{Fs2ServerBackend, RequestResponseMetadata, ServerServiceBuilder}

object Fs2BackendSpec extends ZIOSpecDefault {

  def processComplexRequestFs2(req: ComplexRequest): IO[ComplexResponse] =
    IO.pure(processComplexRequest(req))

  def processWithMetadataFs2(req: MetadataRequest, ctx: RequestResponseMetadata): IO[MetadataResponse] =
    IO.pure(processWithMetadata(req, ctx))

  def spec = suite("Fs2BackendSpec")(
    test("should discover services via gRPC reflection") {
      val result = Dispatcher.parallel[IO].use { dispatcher =>
        val backend = Fs2ServerBackend[IO](dispatcher)
        val serverService = ServerServiceBuilder(using backend)
          .rpc(complexRpc, processComplexRequestFs2)
          .build(testService)
        
        IO.pure(testReflection(6998, serverService.definition))
      }.unsafeRunSync()
      
      assertTrue(result)
    },
    test("should handle complex gRPC request/response with fs2 backend") {
      val result = Dispatcher.parallel[IO].use { dispatcher =>
        val backend = Fs2ServerBackend[IO](dispatcher)
        val serverService = ServerServiceBuilder(using backend)
          .rpc(complexRpc, processComplexRequestFs2)
          .build(testService)

        val port = 6999
        val server = NettyServerBuilder.forPort(port).addService(serverService.definition).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", 6999).usePlaintext().build()
        val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

        val program = for {
          clientFactory1 <- clientBackend.client(testService, complexRpc)
          clientFactory2 <- clientBackend.client(testService, complexRpc)
          clientFactory3 <- clientBackend.client(testService, complexRpc)
          
          response1 <- clientFactory1(sampleRequest)
          response2 <- clientFactory2(sampleRequest.copy(contact = ContactMethod.Phone("555-0123", "US"), priority = Priority.Low))
          response3 <- clientFactory3(sampleRequest.copy(contact = ContactMethod.Slack("my-workspace", "#general"), count = None))
        } yield (response1, response2, response3)

        program.guarantee {
          IO {
            server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          }
        }
      }.unsafeRunSync()

      assertTrue(validateComplexResponse(result._1, result._2, result._3))
    },
    test("should handle client and server metadata") {
      val result = Dispatcher.parallel[IO].use { dispatcher =>
        val backend = Fs2ServerBackend[IO](dispatcher)
        val metadataServerService = ServerServiceBuilder(using backend)
          .rpcWithContext(metadataRpc, processWithMetadataFs2)
          .build(metadataService)

        val port = 6997
        val server = NettyServerBuilder.forPort(port).addService(metadataServerService.definition).build().start()
        val channel = NettyChannelBuilder.forAddress("localhost", 6997).usePlaintext().build()
        val clientBackend = new Fs2ClientBackend[IO](channel, dispatcher)

        val requestMetadata = new Metadata()
        requestMetadata.put(Metadata.Key.of("client-id", Metadata.ASCII_STRING_MARSHALLER), "fs2-client-101")
        requestMetadata.put(Metadata.Key.of("user-agent", Metadata.ASCII_STRING_MARSHALLER), "grpc-fs2/1.0")

        val program = for {
          clientFactory <- clientBackend.clientWithMetadata(metadataService, metadataRpc)
          (response, responseMetadata) <- clientFactory(MetadataRequest("hello fs2 metadata"), requestMetadata)
        } yield (response, responseMetadata)

        program.guarantee {
          IO {
            server.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
            channel.shutdown().awaitTermination(5, TimeUnit.SECONDS): Unit
          }
        }
      }.unsafeRunSync()

      assertTrue(validateMetadataResponse(result._1, result._2, "fs2-client-101", "hello fs2 metadata"))
    }
  )
}