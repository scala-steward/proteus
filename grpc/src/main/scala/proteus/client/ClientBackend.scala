package proteus
package client

import io.grpc.*

trait ClientBackendUnary[Unary[_]] {
  def client[Rpcs, Request, Response](rpc: Rpc.Unary[Request, Response], service: Service[Rpcs & rpc.type]): Unary[Request => Unary[Response]] =
    client(rpc, service, identity)
  def client[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[Request => Unary[Response]]
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[(Request, Metadata) => Unary[(Response, Metadata)]] =
    clientWithMetadata(rpc, service, identity)
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.Unary[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions = identity
  ): Unary[(Request, Metadata) => Unary[(Response, Metadata)]]
}

trait ClientBackend[Unary[_], Streaming[_]] extends ClientBackendUnary[Unary] {
  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[Streaming[Request] => Unary[Response]] =
    client(rpc, service, identity)
  def client[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[Streaming[Request] => Unary[Response]]
  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[Request => Streaming[Response]] = client(rpc, service, identity)
  def client[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[Request => Streaming[Response]]
  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[Streaming[Request] => Streaming[Response]] = client(rpc, service, identity)
  def client[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[Streaming[Request] => Streaming[Response]]
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]] = clientWithMetadata(rpc, service, identity)
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ClientStreaming[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[(Streaming[Request], Metadata) => Unary[(Response, Metadata)]]
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[(Request, Metadata) => Streaming[Response]] = clientWithMetadata(rpc, service, identity)
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.ServerStreaming[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[(Request, Metadata) => Streaming[Response]]
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs & rpc.type]
  ): Unary[(Streaming[Request], Metadata) => Streaming[Response]] = clientWithMetadata(rpc, service, identity)
  def clientWithMetadata[Rpcs, Request, Response](
    rpc: Rpc.BidiStreaming[Request, Response],
    service: Service[Rpcs & rpc.type],
    options: CallOptions => CallOptions
  ): Unary[(Streaming[Request], Metadata) => Streaming[Response]]
}
