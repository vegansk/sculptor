package sbtsculptor.scalagen

import sbt._
import sculptor.scalagen

object SculptorScalagenPlugin extends AutoPlugin {

  object autoImport {

    type ScalagenOptions = scalagen.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenOptions = scalagen.Config

    type ScalagenType = scalagen.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenType = scalagen.Type

    type ScalagenImport = scalagen.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenImport = scalagen.Import

    type ScalagenParameters = scalagen.Parameters
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenParameters = scalagen.Parameters

    final case class ScalagenConfig(xsdFile: File,
                                    outFile: File,
                                    options: ScalagenOptions)

    val scalagenConfigurations: SettingKey[Seq[ScalagenConfig]] = settingKey(
      "List of scalagen configurations"
    )

    val scalagenGenerate: TaskKey[Unit] = taskKey(
      "Generate scala sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] = Seq(
    scalagenConfigurations := Seq(),
    scalagenGenerate := scalagenGenerateTask.value
  )

  override lazy val projectSettings = baseSettings

  private def generate(cfg: ScalagenConfig): Unit = {
    val _ = cfg.outFile.getParentFile.mkdirs
    scalagen
      .generateFromFile(cfg.xsdFile, cfg.outFile, cfg.options)
      .unsafeRunSync
  }

  private lazy val scalagenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    scalagenConfigurations.value.foreach(generate)
  }
}
