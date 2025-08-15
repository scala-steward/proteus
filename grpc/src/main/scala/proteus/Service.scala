package proteus

import scala.jdk.CollectionConverters.*

import com.google.protobuf.DescriptorProtos.*
import com.google.protobuf.Descriptors.FileDescriptor

case class Service[Rpcs] private (name: String, rpcs: List[Rpc[?, ?]], imports: List[Import]) {
  val toProtoIR: List[ProtoIR.TopLevelDef] =
    (ProtoIR.TopLevelDef.ServiceDef(ProtoIR.Service(name, rpcs.map(_.toProtoIR))) ::
      rpcs.flatMap(_.messagesToProtoIR)).distinct

  def fileDescriptor(dependencies: List[Dependency[?]]): FileDescriptor = {
    val fileBuilder = FileDescriptorProto.newBuilder().setName(s"${name.toLowerCase}.proto").setPackage("")

    val dependencyFileDescriptors = dependencies.flatMap(_.fileDescriptor)

    dependencyFileDescriptors.foreach(fileDescriptor => fileBuilder.addDependency(fileDescriptor.getName))

    val dependencyTypes =
      dependencyFileDescriptors.flatMap(_.getMessageTypes.asScala).map(_.getName).toSet ++
        dependencyFileDescriptors.flatMap(_.getEnumTypes.asScala).map(_.getName).toSet

    toProtoIR.foreach {
      case ProtoIR.TopLevelDef.MessageDef(msg)     => if (!dependencyTypes.contains(msg.name)) fileBuilder.addMessageType(msg.toDescriptor): Unit
      case ProtoIR.TopLevelDef.EnumDef(enumDef)    => if (!dependencyTypes.contains(enumDef.name)) fileBuilder.addEnumType(enumDef.toDescriptor): Unit
      case ProtoIR.TopLevelDef.ServiceDef(service) => fileBuilder.addService(service.toDescriptor)
    }
    FileDescriptor.buildFrom(fileBuilder.build(), dependencyFileDescriptors.toArray)
  }

  def imports(`import`: Import): Service[Rpcs & `import`.type] =
    copy(imports = this.imports :+ `import`)

  def rpc[Request, Response](rpc: Rpc[Request, Response]): Service[Rpcs & rpc.type] =
    Service(name, rpcs :+ rpc, imports)

  def render(packageName: Option[String], options: List[ProtoIR.TopLevelOption]): String =
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = imports.map(i => ProtoIR.Statement.ImportStatement(i.name)) ++
          toProtoIR.map(ProtoIR.Statement.TopLevelStatement(_))
      )
    )
}

object Service {
  def apply(name: String): Service[Any] =
    Service(name, List.empty, List.empty)
}
