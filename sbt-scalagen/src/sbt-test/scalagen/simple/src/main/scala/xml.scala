package polaris.xml

import java.time.LocalDate

import kantan.xpath.{NodeDecoder, DecodeError, codecs}
import kantan.xpath.implicits._
import kantan.codecs.Result
import kantan.codecs.strings.StringDecoder

object instances {

  implicit val LocalDateNodeDecoder: NodeDecoder[LocalDate] =
    NodeDecoder[String].emap(s => Result.nonFatal(LocalDate.parse(s)).leftMap(e => DecodeError.TypeError(e.getMessage)))

}
