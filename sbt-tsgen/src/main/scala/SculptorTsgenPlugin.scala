package sbtsculptor.tsgen

import sbt._
import sculptor.tsgen

object SculptorTsgenPlugin extends AutoPlugin {

  object autoImport {

    type TsgenOptions = tsgen.deprecated.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenOptions = tsgen.deprecated.Config

    type TsgenType = tsgen.deprecated.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenType = tsgen.deprecated.Type

    type TsgenImport = tsgen.deprecated.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenImport = tsgen.deprecated.Import

    final case class TsgenConfig(xsdFile: File,
                                 outFile: File,
                                 options: TsgenOptions)

    val tsgenConfigurations: SettingKey[Seq[TsgenConfig]] = settingKey(
      "List of tsgen.deprecated configurations"
    )

    val tsgenGenerate: TaskKey[Unit] = taskKey(
      "Generate typescript sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] =
    Seq(tsgenConfigurations := Seq(), tsgenGenerate := tsgenGenerateTask.value)

  override lazy val projectSettings = baseSettings

  private def generate(cfg: TsgenConfig): Unit = {
    val _ = cfg.outFile.getParentFile.mkdirs
    tsgen.deprecated
      .generateFromFile(cfg.xsdFile, cfg.outFile, cfg.options)
      .unsafeRunSync
  }

  private lazy val tsgenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    tsgenConfigurations.value.foreach(generate)
  }
}
