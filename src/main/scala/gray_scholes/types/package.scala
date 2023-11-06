package gray_scholes

import io.circe.{Decoder, HCursor}

import java.time.Instant

package object types {
  implicit val InstantMsDecoder: Decoder[Instant] = (c: HCursor) => {
    for {
      ms <- c.as[Long]
    } yield {
      Instant.ofEpochMilli(ms)
    }
  }
}
