package proteus
package json

import com.github.plokhotnyuk.jsoniter_scala.core.*
import zio.blocks.schema.{PrimitiveType, Schema}
import zio.blocks.schema.binding.*
import zio.blocks.schema.binding.RegisterOffset.RegisterOffset

import proteus.ProtobufCodec.*
import proteus.ProtobufCodec.MessageField.*
import proteus.internal.*

implicit def jsonWriterCodec[A](using schema: Schema[A], deriver: ProtobufDeriver): JsonValueCodec[A] =
  new JsonValueCodec[A] {
    def nullValue: A                               = null.asInstanceOf[A]
    def decodeValue(in: JsonReader, default: A): A = throw new Exception("Not implemented")
    def encodeValue(a: A, out: JsonWriter): Unit   =
      withRegisters { registers =>
        def loop[B](b: B, codec: ProtobufCodec[B], offset: RegisterOffset, isKey: Boolean): Unit =
          codec match {
            case c: Primitive[_]         =>
              c.primitiveType match {
                case _: PrimitiveType.Int     => if (isKey) out.writeKey(b) else out.writeVal(b)
                case _: PrimitiveType.Long    => if (isKey) out.writeKey(b) else out.writeVal(b)
                case _: PrimitiveType.Boolean => if (isKey) out.writeKey(b) else out.writeVal(b)
                case _: PrimitiveType.String  => if (isKey) out.writeKey(b) else out.writeVal(b)
                case _: PrimitiveType.Double  => if (isKey) out.writeKey(b) else out.writeVal(b)
                case _: PrimitiveType.Float   => if (isKey) out.writeKey(b) else out.writeVal(b)
                case _                        => throw new Exception(s"Unsupported primitive type: ${c.primitiveType}")
              }
            case c: Enum[_]              => if (isKey) out.writeKey(c.namesByValue(b)) else out.writeVal(c.namesByValue(b))
            case c: Message[_]           =>
              out.writeObjectStart()
              c.deconstructor.deconstruct(registers, offset, b)
              val nextOffset = RegisterOffset.add(offset, c.usedRegisters)
              var i          = 0
              while (i < c.fields.length) {
                val field = c.fields(i)
                field match {
                  case f: SimpleField[?] =>
                    out.writeKey(toCamelCase(f.name))
                    loop(getFromRegister(registers, offset, f.register).asInstanceOf[f.codec.Focus], f.codec, nextOffset, isKey = false)
                  case f: OneofField[b]  =>
                    val v     = getFromRegister(registers, offset, f.register).asInstanceOf[b]
                    val index = f.discriminator.discriminate(v)
                    val c     = f.cases(index)
                    loop(v.asInstanceOf[c.codec.Focus], c.codec, nextOffset, isKey = false)
                }
                i += 1
              }
              out.writeObjectEnd()
            case c: Repeated[c, e]       =>
              out.writeArrayStart()
              val it = c.deconstructor.deconstruct[e](b)
              while (it.hasNext) {
                val v = it.next
                loop(v, c.element, offset, isKey = false)
              }
              out.writeArrayEnd()
            case c: RepeatedMap[m, k, v] =>
              out.writeArrayStart()
              val it = c.deconstructor.deconstruct(b.asInstanceOf[m[k, v]])
              while (it.hasNext) {
                val v = it.next
                out.writeObjectStart()
                loop(c.deconstructor.getKey(v), c.element.fields(0).asInstanceOf[SimpleField[?]].codec, offset, isKey = true)
                loop(c.deconstructor.getValue(v), c.element.fields(1).asInstanceOf[SimpleField[?]].codec, offset, isKey = false)
                out.writeObjectEnd()
              }
              out.writeArrayEnd()
            case Bytes                   => out.writeVal("<bytes>")
            case c: Transform[_, _]      => loop(c.to(b), c.codec, offset, isKey = isKey)
            case c: RecursiveMessage[_]  => loop(b, c.codec, offset, isKey = isKey)
            case c: Optional[_]          =>
              b match {
                case None    => out.writeNull()
                case Some(v) => loop(v, c.codec, offset, isKey = isKey)
              }
          }
        val codec                                                                                = schema.derive(deriver)
        loop(a, codec, RegisterOffset.Zero, isKey = false)
      }
  }
