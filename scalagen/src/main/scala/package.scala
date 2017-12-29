package sculptor

import java.io.File
import cats.effect._
import scala.xml._
import java.nio.file.{Paths, Files, StandardOpenOption}
import java.nio.charset.StandardCharsets

package scalagen {
  final case class Import(path: String)

  final case class Type(xsdName: String, name: String)

  final case class Config(packageName: Option[String] = None,
                          imports: List[Import] = Nil,
                          types: List[Type] = Nil,
                          header: Option[String] = None,
                          parameters: Parameters = Parameters())
}

package object scalagen {

  import ast._
  import sculptor.xsd.{ast => x, parseSchema}

  type Parameters = generator.Parameters
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  val Parameters = generator.Parameters

  type OptionalTypes = generator.OptionalTypes
  @SuppressWarnings(Array("org.wartremover.warts.PublicInference"))
  val OptionalTypes = generator.OptionalTypes

  private def toXsdConfig(cfg: Config, xsdNs: Option[String]): xsd.Config =
    xsd.Config(
      imports = cfg.imports.map { i =>
        ImportDecl(i.path)
      },
      xsdNs = xsdNs,
      externalTypes = cfg.types.map { t =>
        xsd.ExternalType(
          xsdName = x.QName.fromString(t.xsdName),
          name = QName.fromString(t.name)
        )
      }
    )

  private def toGeneratorConfig(cfg: Config): generator.Config = {
    generator.Config(
      packageName = cfg.packageName,
      header = cfg.header,
      externalTypes = cfg.types.map(t => generator.ExternalType(t.name)),
      parameters = cfg.parameters
    )
  }

  def generateFromFile(xsdFile: File, tsFile: File, cfg: Config): IO[Unit] =
    for {
      n <- IO[Node] { XML.loadFile(xsdFile) }
      ts <- parseSchema(n)
        .fold[IO[String]](
          err => IO.raiseError(new Exception(err.toList.mkString(", "))),
          res => {
            xsd.transform(res.ast).value(toXsdConfig(cfg, res.schemaNs)) match {
              case Right(module) =>
                IO.pure {
                  generator
                    .create(toGeneratorConfig(cfg))
                    .moduleDecl(module)
                    .render(80)
                }
              case Left(err) => IO.raiseError(new Exception(err))
            }
          }
        )
      _ <- IO {
        tsFile.getParentFile.mkdirs
      }
      _ <- IO {
        Files.write(
          Paths.get(tsFile.getPath),
          ts.getBytes(StandardCharsets.UTF_8),
          StandardOpenOption.CREATE,
          StandardOpenOption.TRUNCATE_EXISTING,
        )
      }
    } yield ()

}
