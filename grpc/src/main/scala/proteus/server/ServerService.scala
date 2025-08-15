package proteus
package server

import com.google.protobuf.Descriptors.FileDescriptor
import io.grpc.*
import io.grpc.protobuf.ProtoFileDescriptorSupplier

case class ServerService[Unary[_], Streaming[_], Context](
  service: Service[?],
  rpcs: List[ServerRpc[Unary, Streaming, Context, ?, ?]],
  dependencies: List[Dependency]
)(using backend: ServerBackend[Unary, Streaming, Context]) {
  val fileDescriptor: FileDescriptor = service.fileDescriptor(dependencies)

  val methodDescriptors: List[MethodDescriptor[?, ?]] =
    rpcs.sortBy(_.name).map(_.toMethodDescriptor(service.name, fileDescriptor))

  val serviceDescriptor: ServiceDescriptor =
    methodDescriptors
      .foldLeft(
        ServiceDescriptor
          .newBuilder(service.name)
          .setSchemaDescriptor(new ProtoFileDescriptorSupplier {
            def getFileDescriptor: FileDescriptor = fileDescriptor
          })
      )((builder, methodDescriptor) => builder.addMethod(methodDescriptor))
      .build()

  lazy val definition: ServerServiceDefinition =
    (methodDescriptors zip rpcs.sortBy(_.name))
      .foldLeft(ServerServiceDefinition.builder(serviceDescriptor)) { case (builder, (methodDescriptor, rpc)) =>
        builder.addMethod[rpc.Request, rpc.Response](
          methodDescriptor.asInstanceOf[MethodDescriptor[rpc.Request, rpc.Response]],
          backend.handler(rpc)
        )
      }
      .build()
}
