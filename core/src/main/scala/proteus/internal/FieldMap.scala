package proteus
package internal

import scala.collection.immutable.HashMap

import proteus.ProtobufCodec.MessageField.*
import proteus.internal.FieldMap.FieldMapEntry

final private[proteus] class FieldMap private (
  private val array: Array[FieldMapEntry],
  private val map: HashMap[Int, FieldMapEntry],
  private val arraySize: Int,
  val indexes: List[Int]
) {
  def get(id: Int): FieldMapEntry =
    if (id >= 0 && id < arraySize) {
      val field = array(id)
      if (field != null) field else map.getOrElse(id, null)
    } else map.getOrElse(id, null)
}
object FieldMap {
  case class FieldMapEntry(field: SimpleField[?], index: Int)

  def apply(fields: HashMap[Int, FieldMapEntry]): FieldMap = {
    // Find the highest field ID that's less than 1000 (typical protobuf field numbers are small)
    val maxId     = fields.keysIterator.filter(_ < 1000).maxOption.getOrElse(0)
    val arraySize = maxId + 1
    val array     = new Array[FieldMapEntry](arraySize)
    val builder   = List.newBuilder[Int]

    // Fill array with fields that have small IDs
    fields.foreach { case (id, field) =>
      if (id < arraySize) array(id) = field
      builder += id
    }

    // Keep only fields with large IDs in the HashMap
    val largeFields = fields.filter { case (id, _) => id >= arraySize }

    new FieldMap(array, largeFields, arraySize, builder.result())
  }
}
