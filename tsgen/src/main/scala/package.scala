package sculptor

import java.nio.charset.StandardCharsets
import java.nio.file.{Files, Paths, StandardOpenOption}

import cats.effect._
import cats.implicits._
import org.typelevel.paiges.Doc
import sculptor.ast.Package

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

  type IotsMapping = PartialFunction[(String, String), String]

  def StdIotsMappings: IotsMapping = {
    case ("null", ns) => s"${ns}null"
    case ("undefined", ns) => s"${ns}undefined"
    case ("void", ns) => s"${ns}void"
    case ("string", ns) => s"${ns}string"
    case ("number", ns) => s"${ns}number"
    case ("boolean", ns) => s"${ns}boolean"
    case ("any", ns) => s"${ns}any"
    case ("never", ns) => s"${ns}never"
    case ("object", ns) => s"${ns}object"
    case ("Array", ns) => s"${ns}array"
  }
}
