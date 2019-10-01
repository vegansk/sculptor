package sbtsculptor

import sbt._
import sculptor.scalagen
import sculptor.tsgen
import sculptor.ast

object SculptorPlugin extends AutoPlugin {

  override val requires = Plugins.empty

  object autoImport {

    object Scala {
      type Config = scalagen.Config
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val Config = scalagen.Config

      type Feature = scalagen.Feature
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val Feature = scalagen.Feature
    }

    final case class ScalagenConfig(pkg: ast.Package,
                                    cfg: Scala.Config,
                                    outFile: File)

    lazy val scalagenConfigurations: SettingKey[Seq[ScalagenConfig]] =
      settingKey("List of scalagen configurations")

    val scalagenGenerate: TaskKey[Unit] = taskKey(
      "Generate scala sources from dsl"
    )

    object TypeScript {
      type Config = tsgen.Config
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val Config = tsgen.Config

      type Feature = tsgen.Feature
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val Feature = tsgen.Feature

      type OptionalEncoding = tsgen.OptionalEncoding
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val OptionalEncoding = tsgen.OptionalEncoding

      type IotsName = tsgen.IotsName

      type IotsMapping = tsgen.IotsMapping

      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val StdIotsMappings = tsgen.StdIotsMappings
    }

    final case class TsgenConfig(pkg: ast.Package,
                                 cfg: TypeScript.Config,
                                 outFile: File)

    lazy val tsgenConfigurations: SettingKey[Seq[TsgenConfig]] =
      settingKey("List of tsgen configurations")

    val tsgenGenerate: TaskKey[Unit] = taskKey(
      "Generate typescript sources from dsl"
    )

  }

  import autoImport._

  def baseSettings: Seq[Setting[_]] = Seq(
    scalagenConfigurations := Seq(),
    scalagenGenerate := scalagenGenerateTask.value,
    tsgenConfigurations := Seq(),
    tsgenGenerate := tsgenGenerateTask.value
  )

  override lazy val projectSettings = baseSettings

  private def generateScala(cfg: ScalagenConfig): Unit = {
    val _ = cfg.outFile.getParentFile.mkdirs
    scalagen
      .generateFile(cfg.pkg, cfg.cfg, cfg.outFile)
      .unsafeRunSync
  }

  private lazy val scalagenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    scalagenConfigurations.value.foreach(generateScala)
  }

  private def generateTs(cfg: TsgenConfig): Unit = {
    val _ = cfg.outFile.getParentFile.mkdirs
    tsgen
      .generateFile(cfg.pkg, cfg.cfg, cfg.outFile)
      .unsafeRunSync
  }

  private lazy val tsgenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    tsgenConfigurations.value.foreach(generateTs)
  }

}
