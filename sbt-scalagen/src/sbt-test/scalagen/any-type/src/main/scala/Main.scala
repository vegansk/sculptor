import kantan.xpath.implicits._
import cats.implicits._
import scala.xml._

object Main extends App {

  override def main(args: Array[String]): Unit = {
    val impl: Either[Throwable, Unit] = for {
      xml <- Either.catchNonFatal(
        XML.load(
        getClass.getClassLoader.getResourceAsStream("MeetingNotification.xml")
      ))
      doc <- xml.toString.evalXPath[types.nsd.MeetingNotification](xp"/MeetingNotification")
      _ = doc.document.mtgNtfctn.xtnsn.foreach(v => println(v.xtnsnEnvlp.value))
    } yield ()
    impl.toTry.get
  }

}
