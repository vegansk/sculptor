package sculptor

import org.typelevel.paiges.Doc
import cats.implicits._
import cats.effect._
import java.nio.file.{Paths, Files, StandardOpenOption}
import java.nio.charset.StandardCharsets

import ast.{Package, TypeRef}

package object tsgen {

  type Result[A] = Either[String, A]

  def generateDoc(p: Package, c: Config): Result[Doc] =
    run(impl.PackageGen.generate(p: Package), c)

  def generateString(p: Package, c: Config): Result[String] =
    generateDoc(p, c).map(_.render(Config.lineWidth(c)))

  def generateFile(p: Package, c: Config, f: java.io.File): IO[Unit] =
    for {
      code <- IO.fromEither(generateString(p, c).leftMap(new Exception(_)))
      _ <- IO {
        f.getParentFile.mkdirs
      }
      _ <- IO {
        Files.write(
          Paths.get(f.getPath),
          code.getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )
      }
    } yield ()

  private[tsgen] def run[A](v: impl.Result[A], c: Config): Result[A] =
    v.value.runA(impl.GeneratorState.init(c)).value

  type IotsName = String

  type IotsMapping = Feature.IoTsTypes => PartialFunction[TypeRef, IotsName]

  def StdIotsMappings: IotsMapping = cfg => {
    val ns = if (cfg.iotsNs.isEmpty) "" else s"${cfg.iotsNs}."
    val result: Function1[TypeRef, Option[String]] = (TypeRef
      .cata(_.name.mkString("."), _.name.name) _) map {
      (((_: String) match {
        case "null" => s"{ns}null"
        case "undefined" => s"${ns}undefined"
        case "void" => s"${ns}void"
        case "string" => s"${ns}string"
        case "number" => s"${ns}number"
        case "boolean" => s"${ns}boolean"
        case "any" => s"${ns}any"
        case "never" => s"${ns}never"
        case "object" => s"${ns}object"
      }): PartialFunction[String, String]).lift
    }

    Function.unlift(result)
  }
}
