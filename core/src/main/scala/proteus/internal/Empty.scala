package proteus
package internal

import zio.blocks.schema.*

private[proteus] case class Empty() derives Schema

private[proteus] object Empty {
  val emptyCodec = Schema[Empty].derive(ProtobufDeriver())
}
