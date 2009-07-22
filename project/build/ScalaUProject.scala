
import sbt._

final class ScalaUProject(info: ProjectInfo) extends DefaultProject(info) {
  
  override def compileOptions = Unchecked :: super.compileOptions.toList
  
}