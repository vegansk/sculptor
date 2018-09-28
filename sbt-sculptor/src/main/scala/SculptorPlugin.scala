package sbtsculptor

import sbt._
import sculptor.scalagen
import sculptor.ast

object SculptorPlugin extends AutoPlugin {

  object autoImport {

    object Scala {
      type Config = scalagen.Config
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val Config = scalagen.Config

      type Feature = scalagen.Feature
      @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
      val Feature = scalagen.Config
    }

    final case class ScalagenConfig(pkg: ast.Package,
                                    cfg: Scala.Config,
                                    outFile: File)

    lazy val scalagenConfigurations: SettingKey[Seq[ScalagenConfig]] =
      settingKey("List of scalagen configurations")

    val scalagenGenerate: TaskKey[Unit] = taskKey(
      "Generate scala sources from dsl"
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
      .generateFile(cfg.pkg, cfg.cfg, cfg.outFile)
      .unsafeRunSync
  }

  private lazy val scalagenGenerateTask: Def.Initialize[Task[Unit]] = Def.task {
    scalagenConfigurations.value.foreach(generate)
  }

}
