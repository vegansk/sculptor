package sbtsculptor.tsgen

import sbt._
import sculptor.tsgen

object SculptorTsgenPlugin extends AutoPlugin {

  object autoImport {

    type XsdTsgenOptions = tsgen.deprecated.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdTsgenOptions = tsgen.deprecated.Config

    type XsdTsgenType = tsgen.deprecated.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdTsgenType = tsgen.deprecated.Type

    type XsdTsgenImport = tsgen.deprecated.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val XsdTsgenImport = tsgen.deprecated.Import

    final case class XsdTsgenConfig(xsdFile: File,
                                    outFile: File,
                                    options: XsdTsgenOptions)

    val xsdTsgenConfigurations: SettingKey[Seq[XsdTsgenConfig]] = settingKey(
      "List of tsgen configurations"
    )

    val xsdTsgenGenerate: TaskKey[Unit] = taskKey(
      "Generate typescript sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] =
    Seq(
      xsdTsgenConfigurations := Seq(),
      xsdTsgenGenerate := xsdTsgenGenerateTask.value
    )

  override lazy val projectSettings = baseSettings

  private def generate(cfg: XsdTsgenConfig): Unit = {
    val _ = cfg.outFile.getParentFile.mkdirs
    tsgen.deprecated
      .generateFromFile(cfg.xsdFile, cfg.outFile, cfg.options)
      .unsafeRunSync
  }

  private lazy val xsdTsgenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    xsdTsgenConfigurations.value.foreach(generate)
  }
}
