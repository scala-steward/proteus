package proteus

import zio.blocks.schema.Modifier

import proteus.internal.*

object Modifiers {
  val excluded                   = Modifier.config(excludedModifier, "true")
  val nested                     = Modifier.config(nestedModifier, "true")
  val oneOf                      = Modifier.config(oneOfModifier, "")
  def rename(name: String)       = Modifier.config(renameModifier, name)
  def enumPrefix(prefix: String) = Modifier.config(enumPrefixModifier, prefix)
  def comment(comment: String)   = Modifier.config(commentModifier, comment)
  def reserved(indexes: Int*)    = Modifier.config(reservedModifier, indexes.mkString(","))
  def oneOf(flags: OneOfFlag*)   = Modifier.config(oneOfModifier, flags.mkString(",").toLowerCase)

  enum OneOfFlag {
    case Inline
    case Nested
  }
}
