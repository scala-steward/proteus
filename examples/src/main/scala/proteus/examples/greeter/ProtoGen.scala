package proteus.examples.greeter

import proteus.*

object Protogen extends App {
  greeterService.renderToFile(Nil, "examples/src/main/proto")
}
