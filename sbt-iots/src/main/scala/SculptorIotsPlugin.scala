package sbtsculptor.iots

import sbt._
import sculptor.iots

object SculptorIotsPlugin extends AutoPlugin {

  object autoImport {

    type IotsConfig = iots.Config
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val IotsConfig = iots.Config

    type IotsType = iots.Type
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val IotsType = iots.Type

    type IotsImport = iots.Import
    @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
    val IotsImport = iots.Import

    val iotsXsdFiles: SettingKey[Seq[File]] = settingKey("List of xsd files")
    val iotsTargetDirectory: SettingKey[File] = settingKey(
      "Output directory for generated files"
    )
    val iotsConfig: SettingKey[IotsConfig] = settingKey(
      "Generator configuration"
    )

    val iotsGenerate: TaskKey[Unit] = taskKey(
      "Generate io-ts sources from xsd files"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] = Seq(
    iotsXsdFiles := Seq(),
    iotsTargetDirectory := file("src_managed"),
    iotsConfig := IotsConfig(),
    iotsGenerate := iotsGenerateTask.value
  )

  override lazy val projectSettings = baseSettings

  private def generateFile(xsd: File, output: File, config: IotsConfig): Unit =
    iots.generateFromFile(xsd, output, config).unsafeRunSync

  private def changePathExt(f: File, p: File, ext: String): File = {
    val n0 = f.getName
    val n = n0.lastIndexOf(".") match {
      case -1 => n0
      case i => n0.substring(0, i)
    }

    new File(p, n + "." + ext)
  }

  private lazy val iotsGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    val xsdFiles = iotsXsdFiles.value
    val targetDir = iotsTargetDirectory.value
    val config = iotsConfig.value

    xsdFiles.foreach(
      xsd => generateFile(xsd, changePathExt(xsd, targetDir, "ts"), config)
    )
  }
}
