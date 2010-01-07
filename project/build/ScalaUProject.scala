
import sbt._

final class ScalaUProject(info: ProjectInfo) extends DefaultProject(info) {
  
  override def compileOptions = Unchecked :: super.compileOptions.toList
  
  override def testListeners = new FileReportListener(Path.fromString(outputPath, "testresults"), log) :: super.testListeners.toList
  
  val scalaTestDep = "org.scala-tools.testing" % "scalatest" % "0.9.5"

}
