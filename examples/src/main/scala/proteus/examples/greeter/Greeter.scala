package proteus.examples.greeter

import proteus.*

case class HelloRequest(name: String) derives ProtobufCodec
case class HelloReply(message: String) derives ProtobufCodec

given ProtobufDeriver = ProtobufDeriver

val sayHelloRpc = Rpc.unary[HelloRequest, HelloReply]("SayHello")

val greeterService = Service("examples", "Greeter").rpc(sayHelloRpc)
