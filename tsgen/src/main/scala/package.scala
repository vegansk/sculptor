package sculptor

import java.io.File
import cats.effect._
import scala.xml._
import java.nio.file.{Paths, Files, StandardOpenOption}
import java.nio.charset.StandardCharsets

package tsgen {
  final case class Import(name: String, path: String)

  final case class Type(xsdName: String, name: String, constName: String)

  final case class Config(imports: List[Import] = Nil,
                          types: List[Type] = Nil,
                          iotsNs: String = "t",
                          header: Option[String] = None,
                          nativeTypes: Boolean = false,
                          generateComments: Boolean = true,
                          generateEnumsDocumentationGetters: Boolean = false,
                          generatePartialTypes: Boolean = false,
                          generatePartialConstants: Boolean = false)
}

package object tsgen {

  import ast._
  import sculptor.xsd.{ast => x, parseSchema}

  private def toXsdConfig(cfg: Config, xsdNs: Option[String]): xsd.Config =
    xsd.Config(
      imports = cfg.imports.map { i =>
        ImportDecl(Ident(i.name), i.path)
      },
      xsdNs = xsdNs,
      externalTypes = cfg.types.map { t =>
        xsd.ExternatType(
          xsdName = x.QName.fromString(t.xsdName),
          name = QName.fromString(t.name),
          constName = QName.fromString(t.constName)
        )
      }
    )

  private def toGeneratorConfig(cfg: Config): generator.Config = {
    generator.Config(
      iotsNs = Ident(cfg.iotsNs),
      header = cfg.header,
      nativeTypes = cfg.nativeTypes,
      generateComments = cfg.generateComments,
      generateEnumsDocumentationGetters = cfg.generateEnumsDocumentationGetters,
      generatePartialTypes = cfg.generatePartialTypes,
      generatePartialConstants = cfg.generatePartialConstants
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
                    .render(0)
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
