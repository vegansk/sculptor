package corpact.schema

import scala.xml._
import scala.util.Try
import kantan.xpath.{NodeDecoder, DecodeResult}
import org.w3c.dom

object cacodecs {

  private def convertNodeToString(node: dom.Node): String = {
    import javax.xml.transform._
    import javax.xml.transform.dom.DOMSource
    import javax.xml.transform.stream.StreamResult
    import java.io.StringWriter
    val t = TransformerFactory.newInstance().newTransformer()
    t.setOutputProperty(OutputKeys.OMIT_XML_DECLARATION, "yes")
    val sw = new StringWriter()
    t.transform(new DOMSource(node), new StreamResult(sw))
    sw.toString
  }

  implicit val XmlNodeSeqNodeDecoder: NodeDecoder[NodeSeq] = NodeDecoder.fromFound { node =>
    val res = Try {
      val s = convertNodeToString(node)
      NodeSeq.seqToNodeSeq(XML.loadString(s).child)
    }
    DecodeResult.fromTry(res)
  }

}
