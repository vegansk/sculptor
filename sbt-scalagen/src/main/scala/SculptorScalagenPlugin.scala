package sbtsculptor.scalagen

import sbt._
import sculptor.scalagen._

object SculptorScalagenPlugin extends AutoPlugin {

  object autoImport {

    type XsdScalagenOptions = deprecated.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdScalagenOptions = deprecated.Config

    type XsdScalagenType = deprecated.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdScalagenType = deprecated.Type

    type XsdScalagenImport = deprecated.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdScalagenImport = deprecated.Import

    type XsdScalagenOptionalTypes = deprecated.OptionalTypes
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdScalagenOptionalTypes = deprecated.OptionalTypes

    type XsdScalagenParameters = deprecated.Parameters
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdScalagenParameters = deprecated.Parameters

    final case class XsdScalagenConfig(xsdFile: File,
                                       outFile: File,
                                       options: XsdScalagenOptions)

    val xsdScalagenConfigurations: SettingKey[Seq[XsdScalagenConfig]] =
      settingKey("List of scalagen configurations")

    val xsdScalagenGenerate: TaskKey[Unit] = taskKey(
      "Generate scala sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] = Seq(
    xsdScalagenConfigurations := Seq(),
    xsdScalagenGenerate := xsdScalagenGenerateTask.value
  )

  override lazy val projectSettings = baseSettings

  private def generate(cfg: XsdScalagenConfig): Unit = {
    val _ = cfg.outFile.getParentFile.mkdirs
    deprecated
      .generateFromFile(cfg.xsdFile, cfg.outFile, cfg.options)
      .unsafeRunSync
  }

  private lazy val xsdScalagenGenerateTask: Def.Initialize[Task[Unit]] =
    Def.task {
      xsdScalagenConfigurations.value.foreach(generate)
    }
}
