package proteus

import java.nio.charset.StandardCharsets
import java.nio.file.*

import scala.collection.immutable.ListSet

case class Dependency(
  dependencyName: String,
  packageName: Option[String],
  path: Option[String],
  types: ListSet[ProtoIR.TopLevelDef],
  dependencies: List[Dependency]
) {
  val allDependencies: Set[Dependency] = dependencies.toSet ++ dependencies.flatMap(_.allDependencies)

  val typeReferences       = types.flatMap(_.collectTypeReferences).toSet
  val filteredDependencies = allDependencies.filter(_.hasAnyOf(typeReferences))
  val dependencyTypes      = filteredDependencies.flatMap(_.types).toSet
  val filteredTypes        = types -- dependencyTypes

  val topLevelFqns: Map[String, String] =
    (allDependencies.flatMap(_.topLevelFqns) ++ types.map(t => (t.name, packageName.fold("")(_ + ".") + t.name))).toMap

  val toImportStatement: ProtoIR.Statement.ImportStatement =
    ProtoIR.Statement.ImportStatement(s"${path.fold("")(_ + "/")}$dependencyName.proto")

  def add[A](using codec: ProtobufCodec[A]): Dependency = {
    val t = ProtobufCodec.toProtoIR(codec).filter(t => !allDependencies.exists(_.hasAnyOf(Set(t.name))))
    copy(types = types ++ t)
  }

  def hasAnyOf(typeNames: Set[String]): Boolean =
    types.exists(typeDef => typeNames.contains(typeDef.name))

  def dependsOn(dependency: Dependency): Dependency =
    copy(types = types -- dependency.types -- dependency.dependencyTypes, dependencies = dependencies :+ dependency)

  def render(options: List[ProtoIR.TopLevelOption]): String = {
    val conflicts = findConflicts
    if (conflicts.nonEmpty) {
      throw new Exception(
        s"Conflicts found in dependency $dependencyName:\n ${conflicts.map { case (name, defs) => s"- Type `$name` is defined in different ways: \n${defs.mkString("\n")}" }.mkString("\n")}\n"
      )
    }
    Renderer.render(
      ProtoIR.CompilationUnit(
        packageName = packageName,
        options = options,
        statements = filteredDependencies.toList.map(_.toImportStatement) ++
          filteredTypes.map(ProtoIR.Statement.TopLevelStatement(_)).toList
      )
    )
  }

  def renderToFile(options: List[ProtoIR.TopLevelOption], folder: String): Unit = {
    val rendered = render(options)
    val fileName = s"${internal.toSnakeCase(dependencyName)}.proto"
    val fullPath = path.fold(Path.of(folder, fileName))(Path.of(folder, _, fileName))
    Files.createDirectories(fullPath.getParent)
    Files.write(fullPath, rendered.getBytes(StandardCharsets.UTF_8), StandardOpenOption.CREATE, StandardOpenOption.TRUNCATE_EXISTING): Unit
  }

  def findConflicts: Map[String, List[String]] =
    types
      .groupBy(_.name)
      .view
      .mapValues(_.map(Renderer.renderTopLevelDef).map(Text.renderText).toList.distinct)
      .toMap
      .filter((_, values) => values.length > 1)
}

object Dependency {
  def apply(dependencyName: String): Dependency =
    Dependency(dependencyName, None, None, ListSet.empty, Nil)

  def apply(dependencyName: String, packageName: String): Dependency =
    Dependency(dependencyName, Some(packageName), None, ListSet.empty, Nil)

  def apply(dependencyName: String, packageName: String, path: String): Dependency =
    Dependency(dependencyName, Some(packageName), Some(path), ListSet.empty, Nil)
}
