package proteus

import com.google.protobuf.DescriptorProtos.FileDescriptorProto
import com.google.protobuf.Descriptors.FileDescriptor
import zio.blocks.schema.Schema

case class Dependency[Import](
  dependencyName: String,
  types: Set[ProtoIR.TopLevelDef] = Set.empty,
  imports: List[ProtoIR.Statement.ImportStatement] = Nil
) {
  val fileDescriptor: Option[FileDescriptor] =
    if (types.nonEmpty) {
      val sharedFileBuilder = FileDescriptorProto.newBuilder().setName(s"$dependencyName").setPackage("")
      types.foreach {
        case ProtoIR.TopLevelDef.MessageDef(msg)  => sharedFileBuilder.addMessageType(msg.toDescriptor)
        case ProtoIR.TopLevelDef.EnumDef(enumDef) => sharedFileBuilder.addEnumType(enumDef.toDescriptor)
        case ProtoIR.TopLevelDef.ServiceDef(_)    =>
      }
      Some(FileDescriptor.buildFrom(sharedFileBuilder.build(), Array.empty))
    } else None

  def add[A: Schema]: Dependency[Import] =
    add(Schema[A].derive(ProtobufDeriver))

  def add[A](codec: ProtobufCodec[A]): Dependency[Import] =
    copy(types = types ++ ProtobufCodec.toProtoIR(codec))

  def dependsOn(dependency: Dependency[?]): Dependency[Import] =
    copy(types = types -- dependency.types, imports = ProtoIR.Statement.ImportStatement(dependency.dependencyName) :: imports)

  def render(packageName: Option[String], options: List[ProtoIR.TopLevelOption]): String =
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = imports ++ types.map(ProtoIR.Statement.TopLevelStatement(_)).toList.sortBy(_.s.name)
      )
    )
}
