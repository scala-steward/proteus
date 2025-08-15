package proteus

case class Import(name: String) {
  def toDependency: Dependency[this.type] =
    Dependency(name)

  def toDependency(services: Service[?]*): Dependency[this.type] = {
    val allTypes = services.flatMap(_.toProtoIR).toSet

    val requestResponseTypeNames =
      services.flatMap(_.rpcs.flatMap(rpc => List(rpc.toProtoIR.request.fqn.name, rpc.toProtoIR.response.fqn.name))).toSet

    val commonTypes = allTypes.filterNot {
      case ProtoIR.TopLevelDef.MessageDef(msg)  => requestResponseTypeNames.contains(msg.name)
      case ProtoIR.TopLevelDef.EnumDef(enumDef) => requestResponseTypeNames.contains(enumDef.name)
      case ProtoIR.TopLevelDef.ServiceDef(_)    => true
    }

    Dependency(name, commonTypes)
  }
}
