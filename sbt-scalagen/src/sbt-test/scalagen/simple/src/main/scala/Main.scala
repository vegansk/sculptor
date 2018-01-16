import io.circe._
import io.circe.syntax._
import kantan.xpath.implicits._
import java.util.UUID
import scala.xml._

object Main extends App {

  def xmlFormat = new scala.xml.PrettyPrinter(80, 2)

  def fes2: Unit = {
    import polaris.schema.fes._
    import polaris.schema.fes.{optional => opt}

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

    result.map(xml => println(xmlFormat.format(xml)))
  }

  def ca: Unit = {
    import corpact.schema._

    val xmlCodecs = XmlSerializers(
      localDateToXml = name => date => <v>{date.toString}</v>.copy(label = name),
      localTimeToXml = name => time => <v>{time.toString}</v>.copy(label = name),
      zonedDateTimeToXml = name => dt => <v>{dt.toOffsetDateTime.toString}</v>.copy(label = name),
      nodeSeqToXml = name => ns => <v>{ns}</v>.copy(label = name)
    )

    val xml0 = XML.load(
      getClass.getClassLoader.getResourceAsStream("xml/ca_doc.xml")
    )

    val result = for {
      doc <- xml0.toString.evalXPath[CorporateActionNarrative](xp"/CorporateActionNarrative").toEither
      xml1 <- Right(CorporateActionNarrative.toXml(xmlCodecs)("CorporateActionNarrative")(doc))
      _ <- xml1.toString.evalXPath[CorporateActionNarrative](xp"/CorporateActionNarrative").toEither
    } yield xml1

    result.map(xml => println(xmlFormat.format(xml)))
  }

  fes2
  ca

}
