package proteus
package internal

sealed private[proteus] trait Text

private[proteus] object Text {
  final case class Line(string: String)    extends Text
  final case class Many(texts: List[Text]) extends Text
  final case class Indent(text: Text)      extends Text
  case object NewLine                      extends Text

  def line(string: String): Line = Line(string)

  def emptyLine: NewLine.type = NewLine

  def many(texts: Text*): Many = Many(texts.toList)

  def many(texts: List[Text]): Many = Many(texts)

  def intersperse(m: Many, sep: Text): Text = {
    val buf  = List.newBuilder[Text]
    var rest = m.texts
    while (rest.nonEmpty) {
      buf += rest.head
      rest = rest.tail
      if (rest.nonEmpty) buf += sep
    }
    many(buf.result())
  }

  def maybe(text: Option[Text]): Text.Many = Many(text.toList)

  def indent(texts: Text*): Text.Indent = indent(texts.toList)

  def indent(texts: List[Text]): Text.Indent = Indent(many(texts))

  def toLines(text: Text): List[String] =
    text match {
      case Line(string) => List(string)
      case Many(texts)  => texts.flatMap(toLines)
      case Indent(text) => toLines(text).map(line => if (line.isEmpty) "" else "    " + line)
      case NewLine      => List("")
    }

  def renderText(text: Text): String =
    toLines(text).mkString("\n")
}
