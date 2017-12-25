package polaris.json

import io.circe._
import io.circe.java8.time
import io.circe.syntax._
import java.time.LocalDate

object instances {

  implicit def localDateDecoder: Decoder[LocalDate] =
      time.decodeLocalDateDefault

  implicit def localDateEncoder: Encoder[LocalDate] =
    time.encodeLocalDateDefault


}
