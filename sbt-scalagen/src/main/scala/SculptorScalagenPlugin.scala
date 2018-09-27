package sbtsculptor.scalagen

import sbt._
import sculptor.scalagen._

object SculptorScalagenPlugin extends AutoPlugin {

  object autoImport {

    type ScalagenOptions = deprecated.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenOptions = deprecated.Config

    type ScalagenType = deprecated.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenType = deprecated.Type

    type ScalagenImport = deprecated.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenImport = deprecated.Import

    type ScalagenOptionalTypes = deprecated.OptionalTypes
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenOptionalTypes = deprecated.OptionalTypes

    type ScalagenParameters = deprecated.Parameters
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val ScalagenParameters = deprecated.Parameters

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
    deprecated
      .generateFromFile(cfg.xsdFile, cfg.outFile, cfg.options)
      .unsafeRunSync
  }

  private lazy val scalagenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    scalagenConfigurations.value.foreach(generate)
  }
}
