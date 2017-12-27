package sbtsculptor.scalagen

import sbt._
import sculptor.scalagen

object SculptorScalagenPlugin extends AutoPlugin {

  object autoImport {

    type ScalagenConfig = scalagen.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenConfig = scalagen.Config

    type ScalagenType = scalagen.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenType = scalagen.Type

    type ScalagenImport = scalagen.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenImport = scalagen.Import

    type ScalagenParameters = scalagen.Parameters
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenParameters = scalagen.Parameters

    val scalagenXsdFiles: SettingKey[Seq[File]] = settingKey(
      "List of xsd files"
    )
    val scalagenTargetDirectory: SettingKey[File] = settingKey(
      "Output directory for generated files"
    )
    val scalagenConfig: SettingKey[ScalagenConfig] = settingKey(
      "Generator configuration"
    )

    val scalagenGenerate: TaskKey[Unit] = taskKey(
      "Generate scala sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] = Seq(
    scalagenXsdFiles := Seq(),
    scalagenTargetDirectory := file("src_managed"),
    scalagenConfig := ScalagenConfig(),
    scalagenGenerate := scalagenGenerateTask.value
  )

  override lazy val projectSettings = baseSettings

  private def generateFile(xsd: File,
                           output: File,
                           config: ScalagenConfig): Unit =
    scalagen.generateFromFile(xsd, output, config).unsafeRunSync

  private def changePathExt(f: File, p: File, ext: String): File = {
    val n0 = f.getName
    val n = n0.lastIndexOf(".") match {
      case -1 => n0
      case i => n0.substring(0, i)
    }

    new File(p, n + "." + ext)
  }

  private lazy val scalagenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    val xsdFiles = scalagenXsdFiles.value
    val targetDir = scalagenTargetDirectory.value
    val config = scalagenConfig.value

    xsdFiles.foreach(
      xsd => generateFile(xsd, changePathExt(xsd, targetDir, "scala"), config)
    )
  }
}
