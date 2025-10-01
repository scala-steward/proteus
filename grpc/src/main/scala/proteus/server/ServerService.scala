package proteus
package server

import com.google.protobuf.Descriptors.FileDescriptor
import io.grpc.*
import io.grpc.protobuf.ProtoFileDescriptorSupplier

case class ServerService[Unary[_], Streaming[_], Context, Rpcs] private (
  serverRpcs: List[ServerRpc[Unary, Streaming, Context, ?, ?]]
)(using val backend: ServerBackend[Unary, Streaming, Context]) {
  def rpc[Request, Response](
    rpc: Rpc.Unary[Request, Response],
    logic: Request => Unary[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.Unary(rpc, (req, _) => logic(req)))

  def rpc[Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    logic: Streaming[Request] => Unary[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.ClientStreaming(rpc, (req, _) => logic(req)))

  def rpc[Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    logic: Request => Streaming[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.ServerStreaming(rpc, (req, _) => logic(req)))

  def rpc[Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    logic: Streaming[Request] => Streaming[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.BidiStreaming(rpc, (req, _) => logic(req)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.Unary[Request, Response],
    logic: (Request, Context) => Unary[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.Unary(rpc, logic(_, _)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    logic: (Streaming[Request], Context) => Unary[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.ClientStreaming(rpc, logic(_, _)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    logic: (Request, Context) => Streaming[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.ServerStreaming(rpc, logic(_, _)))

  def rpcWithContext[Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    logic: (Streaming[Request], Context) => Streaming[Response]
  ): ServerService[Unary, Streaming, Context, Rpcs & rpc.type] =
    ServerService(serverRpcs :+ server.ServerRpc.BidiStreaming(rpc, logic(_, _)))

  def build(service: Service[Rpcs]): ServerServiceDefinition = {
    val rpcs                           = serverRpcs.sortBy(_.name)
    val fileDescriptor: FileDescriptor = service.fileDescriptor

    val methodDescriptors: List[MethodDescriptor[?, ?]] =
      rpcs.map(_.toMethodDescriptor(service.name, service.packageName, fileDescriptor))

    val serviceDescriptor: ServiceDescriptor =
      methodDescriptors
        .foldLeft(
          ServiceDescriptor
            .newBuilder(service.fullyQualifiedName)
            .setSchemaDescriptor(new ProtoFileDescriptorSupplier {
              def getFileDescriptor: FileDescriptor = fileDescriptor
            })
        )((builder, methodDescriptor) => builder.addMethod(methodDescriptor))
        .build()

    (methodDescriptors zip rpcs)
      .foldLeft(ServerServiceDefinition.builder(serviceDescriptor)) { case (builder, (methodDescriptor, rpc)) =>
        builder.addMethod[rpc.Request, rpc.Response](
          methodDescriptor.asInstanceOf[MethodDescriptor[rpc.Request, rpc.Response]],
          backend.handler(rpc)
        )
      }
      .build()
  }
}

object ServerService {
  def apply[Unary[_], Streaming[_], Context](
    using backend: ServerBackend[Unary, Streaming, Context]
  ): ServerService[Unary, Streaming, Context, Any] =
    ServerService(Nil)
}
