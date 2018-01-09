package polaris.json

import io.circe._
import io.circe.java8.time
import io.circe.syntax._
import java.time.LocalDate

object instances {

  implicit val localDateDecoder: Decoder[LocalDate] =
      time.decodeLocalDateDefault

  implicit val localDateEncoder: Encoder[LocalDate] =
    time.encodeLocalDateDefault


}
