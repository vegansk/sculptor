package sculptor

import org.typelevel.paiges.Doc
import cats.implicits._
import cats.effect._
import java.nio.file.{Paths, Files, StandardOpenOption}
import java.nio.charset.StandardCharsets

import ast.Package

package object scalagen {

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

  private[scalagen] def run[A](v: impl.Result[A], c: Config): Result[A] =
    v.value.runA(impl.GeneratorState.init(c)).value
}
