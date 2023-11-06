package gray_scholes

import java.time.Instant

sealed trait OptionStyle
case object American extends OptionStyle
case object European extends OptionStyle

case class ManifoldOption(
  style: OptionStyle,
  strike: Double,
  ticker: String,
  expiration: Instant,
)
