import polaris.schema.fes._
import polaris.schema.fes.{optional => opt}

import io.circe._
import io.circe.syntax._
import kantan.xpath.implicits._
import java.util.UUID

object Main extends App {

  val xmlCodecs = XmlSerializers(
    localDateToXml = name => date => <v>{date.toString}</v>.copy(label = name),
    uUIDToXml = name => uuid => <v>{uuid.toString}</v>.copy(label = name)
  )

  val sender = ExchangeParticipantT(
    IdT(UUID.randomUUID),
    ExchangeParticipantTypeEt.RegistrarSubdivision,
    "Sender name"
  )

  val recipient = ExchangeParticipantT(
    IdT(UUID.randomUUID),
    ExchangeParticipantTypeEt.Registrar,
    "Receiver name"
  )

  val messageHeader = MessageHeaderT(
    version = "fes-2.0",
    id = IdT(UUID.randomUUID),
    sender = sender,
    recipient = recipient
  )

  val documentHeader = DocumentHeaderT(
    `type` = DocumentTypeT(
      DocumentTypeEt.ApplicationToOpenLegalAccount,
      None
    ),
    outNum = Some("1/2"),
    outDate = None,
    baseDocument = None
  )

  val content = DocumentContentT(
    unstructured = Some(
      UnstructuredDocumentContentT(
        None,
        Some("Hello, world!")
      )
    ),
    structured = None,
    scanImage = None
  )

  val document = Document(
    messageHeader,
    documentHeader,
    None, None, None, None, None,
    content,
    None, None
  )

  val result = for {
    jsonDoc <- Right(document.asJson)
    optionalDoc <- jsonDoc.as[opt.Document]
    strongDoc <- opt.Document.strong(optionalDoc).toEither
    xml0 <- Right(Document.toXml(xmlCodecs)("DOCUMENT")(strongDoc))
    docFromXml <- xml0.toString.evalXPath[Document](xp"/DOCUMENT").toEither
    xml <- Right(Document.toXml(xmlCodecs)("DOCUMENT")(docFromXml))
  } yield xml

  println(result)

}
