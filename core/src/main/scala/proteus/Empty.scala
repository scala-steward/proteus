package proteus

import zio.blocks.schema.*

case class Empty() derives Schema

object Empty {
  given emptyCodec: ProtobufCodec[Empty] = Schema[Empty].derive(ProtobufDeriver)
}
