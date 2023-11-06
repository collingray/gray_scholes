package gray_scholes.types

import io.circe.{Decoder, HCursor}

import java.time.Instant

/**
 * type LiteMarket = {
 * // Unique identifer for this market
 * id: string
 *
 * // Attributes about the creator
 * creatorUsername: string
 * creatorName: string
 * createdTime: number // milliseconds since epoch
 * creatorAvatarUrl ?: string
 *
 * // Market attributes. All times are in milliseconds since epoch
 * closeTime ?: number // Min of creator's chosen date, and resolutionTime
 * question: string
 *
 * // Note: This url always points to https://manifold.markets, regardless of what instance the api is running on.
 * // This url includes the creator's username, but this doesn't need to be correct when constructing valid URLs.
 * //   i.e. https://manifold.markets/Austin/test-market is the same as https://manifold.markets/foo/test-market
 * url: string
 *
 * outcomeType: string // BINARY, FREE_RESPONSE, MULTIPLE_CHOICE, NUMERIC, or PSEUDO_NUMERIC
 * mechanism: string // dpm-2 or cpmm-1
 *
 * probability: number
 * pool: {outcome: number} // For CPMM markets, the number of shares in the liquidity pool. For DPM markets, the amount of mana invested in each answer.
 * p ?: number // CPMM markets only, probability constant in y^p * n^(1-p) = k
 * totalLiquidity ?: number // CPMM markets only, the amount of mana deposited into the liquidity pool
 * value ?: number // PSEUDO_NUMERIC markets only, the current market value, which is mapped from probability using min, max, and isLogScale.
 * min ?: number // PSEUDO_NUMERIC markets only, the minimum resolvable value
 * max ?: number // PSEUDO_NUMERIC markets only, the maximum resolvable value
 * isLogScale ?: bool // PSEUDO_NUMERIC markets only, if true `number = (max - min + 1)^probability + minstart - 1`, otherwise `number = min + (max - min) * probability`
 *
 * volume: number
 * volume24Hours: number
 *
 * isResolved: boolean
 * resolutionTime ?: number
 * resolution ?: string
 * resolutionProbability ?: number // Used for BINARY markets resolved to MKT
 *
 * lastUpdatedTime ?: number
 * }
 */
case class Market(
  id: String,
  creatorUsername: String,
  creatorName: String,
  createdTime: Instant,
  creatorAvatarUrl: Option[String],
  closeTime: Option[Instant],
  question: String,
  url: String,
  outcomeType: Market.OutcomeType,
  mechanism: Option[Market.Mechanism],
  probability: Option[Double],
  volume: Double,
  volume24Hours: Double,
  isResolved: Boolean,
  resolutionTime: Option[Instant],
  resolution: Option[String],
  resolutionProbability: Option[Double],
  lastUpdatedTime: Option[Instant],
)

object Market {

  sealed trait OutcomeType {
    val id: String
  }
  object OutcomeType {
    case object Binary extends OutcomeType { val id = "BINARY" }
    case object FreeResponse extends OutcomeType { val id = "FREE_RESPONSE" }
    case object MultipleChoice extends OutcomeType { val id = "MULTIPLE_CHOICE" }
    case object Numeric extends OutcomeType { val id = "NUMERIC" }
    case object PseudoNumeric extends OutcomeType { val id = "PSEUDO_NUMERIC" }
    case object Stonk extends OutcomeType { val id = "STONK" }
    case object BountiedQuestion extends OutcomeType { val id = "BOUNTIED_QUESTION" }
    case object Poll extends OutcomeType { val id = "POLL" }
  }

  sealed trait Mechanism {
    val id: String
  }
  object Mechanism {
    case object DPM extends Mechanism { val id = "dpm-2" }
    case object CPMM extends Mechanism { val id = "cpmm-1" }
    case object CPMM_MULTI extends Mechanism { val id = "cpmm-multi-1" }
    case object NoMechanism extends Mechanism { val id = "none" }
  }

  implicit val OutcomeTypeDecoder: Decoder[OutcomeType] = (c: HCursor) => {
    for {
      id <- c.as[String]
    } yield {
      id match {
        case OutcomeType.Binary.id           => OutcomeType.Binary
        case OutcomeType.FreeResponse.id     => OutcomeType.FreeResponse
        case OutcomeType.MultipleChoice.id   => OutcomeType.MultipleChoice
        case OutcomeType.Numeric.id          => OutcomeType.Numeric
        case OutcomeType.PseudoNumeric.id    => OutcomeType.PseudoNumeric
        case OutcomeType.Stonk.id            => OutcomeType.Stonk
        case OutcomeType.BountiedQuestion.id => OutcomeType.BountiedQuestion
        case OutcomeType.Poll.id             => OutcomeType.Poll
      }
    }
  }

  implicit val MechanismDecoder: Decoder[Mechanism] = (c: HCursor) => {
    for {
      id <- c.as[String]
    } yield {
      id match {
        case Mechanism.DPM.id         => Mechanism.DPM
        case Mechanism.CPMM.id        => Mechanism.CPMM
        case Mechanism.CPMM_MULTI.id  => Mechanism.CPMM_MULTI
        case Mechanism.NoMechanism.id => Mechanism.NoMechanism
        case _                        => Mechanism.NoMechanism
      }
    }
  }

  implicit val MarketDecoder: Decoder[Market] = (c: HCursor) => {
    for {
      id <- c.downField("id").as[String]
      creatorUsername <- c.downField("creatorUsername").as[String]
      creatorName <- c.downField("creatorName").as[String]
      createdTime <- c.downField("createdTime").as[Instant]
      creatorAvatarUrl <- c.downField("creatorAvatarUrl").as[Option[String]]
      closeTime <- c.downField("closeTime").as[Option[Instant]]
      question <- c.downField("question").as[String]
      url <- c.downField("url").as[String]
      outcomeType <- c.downField("outcomeType").as[OutcomeType]
      mechanism <- c.downField("mechanism").as[Option[Mechanism]]
      probability <- c.downField("probability").as[Option[Double]]
      volume <- c.downField("volume").as[Double]
      volume24Hours <- c.downField("volume24Hours").as[Double]
      isResolved <- c.downField("isResolved").as[Boolean]
      resolutionTime <- c.downField("resolutionTime").as[Option[Instant]]
      resolution <- c.downField("resolution").as[Option[String]]
      resolutionProbability <- c.downField("resolutionProbability").as[Option[Double]]
      lastUpdatedTime <- c.downField("lastUpdatedTime").as[Option[Instant]]
    } yield {
      Market(
        id,
        creatorUsername,
        creatorName,
        createdTime,
        creatorAvatarUrl,
        closeTime,
        question,
        url,
        outcomeType,
        mechanism,
        probability,
        volume,
        volume24Hours,
        isResolved,
        resolutionTime,
        resolution,
        resolutionProbability,
        lastUpdatedTime,
      )
    }
  }
}
