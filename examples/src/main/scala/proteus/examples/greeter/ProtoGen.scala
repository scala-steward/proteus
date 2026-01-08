package proteus.examples.greeter

import proteus.*

object ProtoGen extends App {
  greeterService.renderToFile(Nil, "examples/src/main/proto")
}
