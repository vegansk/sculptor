package sbtsculptor.tsgen

import sbt._
import sculptor.tsgen

object SculptorTsgenPlugin extends AutoPlugin {

  object autoImport {

    type TsgenOptions = tsgen.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenOptions = tsgen.Config

    type TsgenType = tsgen.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenType = tsgen.Type

    type TsgenImport = tsgen.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenImport = tsgen.Import

    final case class TsgenConfig(xsdFile: File,
                                 outFile: File,
                                 options: TsgenOptions)

    val tsgenConfigurations: SettingKey[Seq[TsgenConfig]] = settingKey(
      "List of tsgen configurations"
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
    tsgen.generateFromFile(cfg.xsdFile, cfg.outFile, cfg.options).unsafeRunSync
  }

  private lazy val tsgenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    tsgenConfigurations.value.foreach(generate)
  }
}
