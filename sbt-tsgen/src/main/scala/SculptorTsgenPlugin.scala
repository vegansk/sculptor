package sbtsculptor.tsgen

import sbt._
import sculptor.tsgen

object SculptorTsgenPlugin extends AutoPlugin {

  object autoImport {

    type TsgenConfig = tsgen.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenConfig = tsgen.Config

    type TsgenType = tsgen.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenType = tsgen.Type

    type TsgenImport = tsgen.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val TsgenImport = tsgen.Import

    val tsgenXsdFiles: SettingKey[Seq[File]] = settingKey("List of xsd files")
    val tsgenTargetDirectory: SettingKey[File] = settingKey(
      "Output directory for generated files"
    )
    val tsgenConfig: SettingKey[TsgenConfig] = settingKey(
      "Generator configuration"
    )

    val tsgenGenerate: TaskKey[Unit] = taskKey(
      "Generate typescript sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] = Seq(
    tsgenXsdFiles := Seq(),
    tsgenTargetDirectory := file("src_managed"),
    tsgenConfig := TsgenConfig(),
    tsgenGenerate := tsgenGenerateTask.value
  )

  override lazy val projectSettings = baseSettings

  private def generateFile(xsd: File, output: File, config: TsgenConfig): Unit =
    tsgen.generateFromFile(xsd, output, config).unsafeRunSync

  private def changePathExt(f: File, p: File, ext: String): File = {
    val n0 = f.getName
    val n = n0.lastIndexOf(".") match {
      case -1 => n0
      case i => n0.substring(0, i)
    }

    new File(p, n + "." + ext)
  }

  private lazy val tsgenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    val xsdFiles = tsgenXsdFiles.value
    val targetDir = tsgenTargetDirectory.value
    val config = tsgenConfig.value

    xsdFiles.foreach(
      xsd => generateFile(xsd, changePathExt(xsd, targetDir, "ts"), config)
    )
  }
}
