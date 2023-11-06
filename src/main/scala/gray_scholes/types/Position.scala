package gray_scholes.types

import io.circe.{Decoder, HCursor}

import java.time.Instant

/**
 * type ContractMetric = {
 *  contractId: string
 *  from:
 *    | {
 *        // includes, day, week,month
 *        [period: string]: {
 *          profit: number
 *          profitPercent: number
 *          invested: number
 *          prevValue: number
 *          value: number
 *        }
 *      }
 *    | undefined
 *  hasNoShares: boolean
 *  hasShares: boolean
 *  hasYesShares: boolean
 *  invested: number
 *  loan: number
 *  maxSharesOutcome: string | null
 *  payout: number
 *  profit: number
 *  profitPercent: number
 *  totalShares: {
 *    [outcome: string]: number
 *  }
 *  userId: string
 *  userUsername: string
 *  userName: string
 *  userAvatarUrl: string
 *  lastBetTime: number
 * }
 */
case class Position(
  contractId: String,
  from: Option[Position.From],
  hasNoShares: Boolean,
  hasShares: Boolean,
  hasYesShares: Boolean,
  invested: Double,
  loan: Double,
  maxSharesOutcome: Option[String],
  payout: Double,
  profit: Double,
  profitPercent: Double,
  totalShares: Map[String, Double],
  userId: String,
  userUsername: String,
  userName: String,
  userAvatarUrl: String,
  lastBetTime: Instant,
)

object Position {

  case class From(
    day: Returns,
    week: Returns,
    month: Returns,
  )

  case class Returns(
    profit: Double,
    profitPercent: Double,
    invested: Double,
    prevValue: Double,
    value: Double,
  )

  implicit val FromDecoder: Decoder[From] = (c: HCursor) => {
    for {
      day <- c.downField("day").as[Returns]
      week <- c.downField("week").as[Returns]
      month <- c.downField("month").as[Returns]
    } yield {
      From(day, week, month)
    }
  }

  implicit val ReturnsDecoder: Decoder[Returns] = (c: HCursor) => {
    for {
      profit <- c.downField("profit").as[Double]
      profitPercent <- c.downField("profitPercent").as[Double]
      invested <- c.downField("invested").as[Double]
      prevValue <- c.downField("prevValue").as[Double]
      value <- c.downField("value").as[Double]
    } yield {
      Returns(profit, profitPercent, invested, prevValue, value)
    }
  }

  implicit val PositionDecoder: Decoder[Position] = (c: HCursor) => {
    for {
      contractId <- c.downField("contractId").as[String]
      from <- c.downField("from").as[Option[From]]
      hasNoShares <- c.downField("hasNoShares").as[Boolean]
      hasShares <- c.downField("hasShares").as[Boolean]
      hasYesShares <- c.downField("hasYesShares").as[Boolean]
      invested <- c.downField("invested").as[Double]
      loan <- c.downField("loan").as[Double]
      maxSharesOutcome <- c.downField("maxSharesOutcome").as[Option[String]]
      payout <- c.downField("payout").as[Double]
      profit <- c.downField("profit").as[Double]
      profitPercent <- c.downField("profitPercent").as[Double]
      totalShares <- c.downField("totalShares").as[Map[String, Double]]
      userId <- c.downField("userId").as[String]
      userUsername <- c.downField("userUsername").as[String]
      userName <- c.downField("userName").as[String]
      userAvatarUrl <- c.downField("userAvatarUrl").as[String]
      lastBetTime <- c.downField("lastBetTime").as[Instant]
    } yield {
      Position(
        contractId,
        from,
        hasNoShares,
        hasShares,
        hasYesShares,
        invested,
        loan,
        maxSharesOutcome,
        payout,
        profit,
        profitPercent,
        totalShares,
        userId,
        userUsername,
        userName,
        userAvatarUrl,
        lastBetTime,
      )
    }
  }
}
