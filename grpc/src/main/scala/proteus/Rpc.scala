package proteus

import com.google.protobuf.Descriptors.{FileDescriptor, MethodDescriptor => PbMethodDescriptor, ServiceDescriptor => PbServiceDescriptor}
import io.grpc.MethodDescriptor
import io.grpc.MethodDescriptor.MethodType
import io.grpc.protobuf.ProtoMethodDescriptorSupplier
import zio.blocks.schema.Schema

sealed trait Rpc[Req, Resp](using requestCodec: ProtobufCodec[Req], responseCodec: ProtobufCodec[Resp]) { self =>
  type Request  = Req
  type Response = Resp

  val name: String

  val toProtoIR: ProtoIR.Rpc =
    ProtoIR.Rpc(
      name = name,
      request = ProtoIR.RpcMessage(ProtoIR.Fqn(None, requestCodec.asInstanceOf[ProtobufCodec.Message[?]].name)),
      response = ProtoIR.RpcMessage(ProtoIR.Fqn(None, responseCodec.asInstanceOf[ProtobufCodec.Message[?]].name)),
      streamingRequest = self match {
        case _: Rpc.ClientStreaming[?, ?] | _: Rpc.BidiStreaming[?, ?] => true
        case _                                                         => false
      },
      streamingResponse = self match {
        case _: Rpc.ServerStreaming[?, ?] | _: Rpc.BidiStreaming[?, ?] => true
        case _                                                         => false
      }
    )

  val messagesToProtoIR: List[ProtoIR.TopLevelDef] =
    ProtobufCodec.toProtoIR(requestCodec) ++ ProtobufCodec.toProtoIR(responseCodec)

  def toMethodDescriptor(serviceName: String, packageName: Option[String], fileDescriptor: FileDescriptor): MethodDescriptor[Request, Response] = {
    val serviceDescriptor = fileDescriptor.findServiceByName(serviceName)
    val methodDescriptor  = serviceDescriptor.findMethodByName(name)

    MethodDescriptor
      .newBuilder()
      .setType(self match {
        case _: Rpc.Unary[?, ?]           => MethodType.UNARY
        case _: Rpc.ClientStreaming[?, ?] => MethodType.CLIENT_STREAMING
        case _: Rpc.ServerStreaming[?, ?] => MethodType.SERVER_STREAMING
        case _: Rpc.BidiStreaming[?, ?]   => MethodType.BIDI_STREAMING
      })
      .setFullMethodName(MethodDescriptor.generateFullMethodName(packageName.fold(serviceName)(s => s"$s.$serviceName"), name))
      .setSampledToLocalTracing(true)
      .setRequestMarshaller(ProtobufMarshaller[Request])
      .setResponseMarshaller(ProtobufMarshaller[Response])
      .setSchemaDescriptor(new ProtoMethodDescriptorSupplier {
        def getMethodDescriptor: PbMethodDescriptor   = methodDescriptor
        def getServiceDescriptor: PbServiceDescriptor = serviceDescriptor
        def getFileDescriptor: FileDescriptor         = fileDescriptor
      })
      .build()
  }
}

object Rpc {
  case class Unary[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]
  case class ClientStreaming[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]
  case class ServerStreaming[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]
  case class BidiStreaming[Request, Response](name: String)(using requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response])
    extends Rpc[Request, Response]

  def unary[Request: Schema, Response: Schema](name: String)(using deriver: ProtobufDeriver): Unary[Request, Response]                               =
    unary(name, Schema[Request].derive(deriver), Schema[Response].derive(deriver))
  def unary[Request, Response](name: String, requestCodec: ProtobufCodec[Request], responseCodec: ProtobufCodec[Response]): Unary[Request, Response] =
    Unary(name)(using requestCodec, responseCodec)

  def clientStreaming[Request: Schema, Response: Schema](name: String)(using deriver: ProtobufDeriver): ClientStreaming[Request, Response] =
    clientStreaming(name, Schema[Request].derive(deriver), Schema[Response].derive(deriver))
  def clientStreaming[Request, Response](
    name: String,
    requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): ClientStreaming[Request, Response] =
    ClientStreaming(name)(using requestCodec, responseCodec)

  def serverStreaming[Request: Schema, Response: Schema](name: String)(using deriver: ProtobufDeriver): ServerStreaming[Request, Response] =
    serverStreaming(name, Schema[Request].derive(deriver), Schema[Response].derive(deriver))
  def serverStreaming[Request, Response](
    name: String,
    requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): ServerStreaming[Request, Response] =
    ServerStreaming(name)(using requestCodec, responseCodec)

  def bidiStreaming[Request: Schema, Response: Schema](name: String)(using deriver: ProtobufDeriver): BidiStreaming[Request, Response] =
    bidiStreaming(name, Schema[Request].derive(deriver), Schema[Response].derive(deriver))
  def bidiStreaming[Request, Response](
    name: String,
    requestCodec: ProtobufCodec[Request],
    responseCodec: ProtobufCodec[Response]
  ): BidiStreaming[Request, Response] =
    BidiStreaming(name)(using requestCodec, responseCodec)
}
