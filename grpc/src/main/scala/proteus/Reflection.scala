package proteus

import zio.blocks.schema.Modifier.config
import zio.blocks.schema.Schema

@config("proteus.reserved", "2")
case class ServerReflectionRequest(host: String, messageRequest: MessageRequest) derives Schema
@config("proteus.reserved", "3")
case class ServerReflectionResponse(validHost: String, originalRequest: ServerReflectionRequest, messageResponse: MessageResponse) derives Schema

@config("proteus.inline", "true")
enum MessageRequest derives Schema {
  @config("proteus.unwrap", "true") case FileByFileName(value: String)
  @config("proteus.unwrap", "true") case FileContainingSymbol(value: String)
  @config("proteus.rename", "ExtensionRequest") case FileContainingExtension(containingType: String, extensionNumber: Int)
  @config("proteus.unwrap", "true") case AllExtensionNumbersOfType(value: String)
  @config("proteus.unwrap", "true") case ListServices(value: String)
}

@config("proteus.inline", "true")
enum MessageResponse derives Schema {
  case FileDescriptorResponse(fileDescriptorProto: String) // TODO: support Array[Byte]
  @config("proteus.rename", "ExtensionNumberResponse") case AllExtensionNumbersResponse(baseTypeName: String, extensionNumber: List[Int])
  @config("proteus.rename", "ListServiceResponse") case ListServicesResponse(service: List[ServiceResponse])
  case ErrorResponse(errorCode: Int, errorMessage: String)
}

case class ServiceResponse(name: String) derives Schema

given deriver: ProtobufDeriver = ProtobufDeriver()

val serverReflectionInfoRpc = Rpc.bidiStreaming[ServerReflectionRequest, ServerReflectionResponse]("ServerReflectionInfo")
val serverReflectionService = Service("grpc.reflection.v1", "ServerReflection").rpc(serverReflectionInfoRpc)

def reflectionClient[Unary[_], Streaming[_]](
  backend: client.ClientBackend[Unary, Streaming]
): Unary[Streaming[ServerReflectionRequest] => Streaming[ServerReflectionResponse]] =
  backend.client(serverReflectionService, serverReflectionInfoRpc)
