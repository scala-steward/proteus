package proteus

import java.nio.charset.StandardCharsets
import java.nio.file.*

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor
import zio.blocks.schema.Schema

case class Dependency(packageName: Option[String], dependencyName: String, types: Set[ProtoIR.TopLevelDef], dependencies: List[Dependency]) {
  val typeReferences       = types.flatMap(_.collectTypeReferences).toSet
  val filteredDependencies = dependencies.filter(_.hasAnyOf(typeReferences))

  val fileDescriptor: Option[FileDescriptor] =
    if (types.nonEmpty) {
      val sharedFileBuilder = FileDescriptorProto.newBuilder().setName(s"$dependencyName").setPackage(packageName.getOrElse(""))
      types.foreach {
        case ProtoIR.TopLevelDef.MessageDef(msg)  => sharedFileBuilder.addMessageType(msg.toDescriptor)
        case ProtoIR.TopLevelDef.EnumDef(enumDef) => sharedFileBuilder.addEnumType(enumDef.toDescriptor)
        case ProtoIR.TopLevelDef.ServiceDef(_)    =>
      }
      Some(FileDescriptor.buildFrom(sharedFileBuilder.build(), filteredDependencies.flatMap(_.fileDescriptor).toArray))
    } else None

  def add[A: Schema](using deriver: ProtobufDeriver): Dependency =
    add(Schema[A].derive(deriver))

  def add[A](codec: ProtobufCodec[A]): Dependency = {
    val t = ProtobufCodec.toProtoIR(codec)
    if (dependencies.exists(_.hasAnyOf(t.map(_.name).toSet))) this
    else copy(types = types ++ t)
  }

  def hasAnyOf(typeNames: Set[String]): Boolean =
    types.exists(typeDef => typeNames.contains(typeDef.name))

  def dependsOn(dependency: Dependency): Dependency =
    copy(types = types -- dependency.types, dependencies = dependencies :+ dependency)

  def render(options: List[ProtoIR.TopLevelOption]): String = {
    val dependencyTypes = filteredDependencies.flatMap(_.types).map(_.name).toSet
    val filteredTypes   = types.filterNot(d => dependencyTypes.contains(d.name))

    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = filteredDependencies.toList.map(d => ProtoIR.Statement.ImportStatement(d.dependencyName)) ++
          filteredTypes.map(ProtoIR.Statement.TopLevelStatement(_)).toList.sortBy(_.s.name)
      )
    )
  }

  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String): Unit = {
    val rendered = render(options)
    val path     = Path.of(folder, s"$dependencyName.proto")
    Files.write(path, rendered.getBytes(StandardCharsets.UTF_8)): Unit
  }
}

object Dependency {
  def apply(dependencyName: String): Dependency =
    Dependency(None, dependencyName, Set.empty, Nil)

  def apply(packageName: String, dependencyName: String): Dependency =
    Dependency(Some(packageName), dependencyName, Set.empty, Nil)
}
