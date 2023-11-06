package gray_scholes

import io.circe.syntax._
import io.circe.parser.decode
import gray_scholes.types.{Bet, Market, Position}

case class ManifoldAPI(
  apiKey: String = ""
) {
  private val baseUrl: String = "https://manifold.markets/api/v0"

  def getMarket(
    marketId: String
  ): Either[String, Market] = {
    val url = s"$baseUrl/market/$marketId"

    val response = requests.get(url)

    response.statusCode match {
      case 200 =>
        val market = response.text()
        decode[Market](market).left.map(_.toString)
      case _ =>
        Left(response.statusMessage)
    }
  }

  def getMarkets(
    limit: Option[Int] = None,
    before: Option[String] = None,
    groupId: Option[String] = None,
  ): Either[String, List[Market]] = {

    val options = List(
      limit.map(l => s"limit=$l"),
      before.map(b => s"before=$b"),
    ).flatten.mkString("?", "&", "")

    val url = groupId match {
      case Some(groupId) => s"$baseUrl/group/by-id/$groupId/markets$options"
      case None          => s"$baseUrl/markets$options"
    }

    val response = requests.get(url)

    response.statusCode match {
      case 200 =>
        val markets = response.text()
        val marketStrings = decode[List[String]](markets).left.map(_.toString)
        marketStrings.map { marketStrings =>
          marketStrings.flatMap { marketString =>
            val res = decode[Market](marketString).left.map(_.toString)

            res match {
              case Left(err) =>
                println("Failed to decode one market: " + err)
                println("Market json: " + marketString)
                None
              case Right(market) => Some(market)
            }
          }
        }
        decode[List[Market]](markets).left.map(_.toString)
      case _ =>
        Left(response.statusMessage)
    }
  }

  /**
   * Get `totalLimit` markets that were created before the market with id `before`, within group `groupId`.
   *
   * If `totalLimit` is none, gets all markets
   */
  def getAllMarkets(
    totalLimit: Option[Int] = None,
    before: Option[String] = None,
    groupId: Option[String] = None,
    innerDelay: Option[Long] = Some(1000),
  ): Either[String, List[Market]] = {
    val maxStepSize = 1000
    val stepSize = totalLimit.filter(_ < maxStepSize).getOrElse(maxStepSize)

    val newBatch = getMarkets(
      limit = Some(stepSize),
      before = before,
      groupId = groupId,
    )

    newBatch.flatMap { batch =>
      if (batch.length < stepSize || totalLimit.exists(_ <= stepSize)) {
        Right(batch)
      } else {
        Thread.sleep(innerDelay.getOrElse(0.toLong))
        getAllMarkets(totalLimit.map(_ - stepSize), Some(batch.last.id), groupId, innerDelay).map(batch ++ _)
      }
    }
  }

  /**
   * Step size should be chosen based on the expected number of markets created per second. If the step size is too
   * small, we will make too many requests. If it is too large, we will be fetching more markets than we need.
   *
   * @return
   */
  def getMarketsSince(
    after: String,
    limit: Option[Int] = None,
    groupId: Option[String] = None,
    stepSize: Int = 50,
    innerDelay: Option[Long] = Some(1000),
  ): Either[String, List[Market]] = getMarketsSinceInner(after, None, limit, groupId, stepSize, innerDelay)

  private def getMarketsSinceInner(
    after: String,
    before: Option[String],
    limit: Option[Int],
    groupId: Option[String],
    stepSize: Int,
    innerDelay: Option[Long],
  ): Either[String, List[Market]] = {

    val newBatch = getMarkets(
      limit = Some(Math.min(stepSize, limit.getOrElse(stepSize))),
      before = before,
      groupId = groupId,
    )

    newBatch.flatMap { batch =>
      Some(batch.indexWhere(_.id == after)).filter(_ >= 0) match {
        // The 'after' element was found in the current batch, we chop the tail (including the after element) and return
        case Some(index) if index < stepSize - 1 =>
          Right(batch.take(index))

        // The 'after' element wasn't found, we recurse with the new 'before' as long as we haven't reached the limit
        case _ =>
          if (batch.length < stepSize || limit.exists(_ <= stepSize)) {
            Right(batch)
          } else {
            Thread.sleep(innerDelay.getOrElse(0.toLong))
            getMarketsSinceInner(after, Some(batch.last.id), limit.map(_ - stepSize), groupId, stepSize, innerDelay)
              .map(batch ++ _)
          }
      }
    }
  }

  /**
   * userId: Optional. If set, the response will include only bets created by this user.
   * username: Optional. If set, the response will include only bets created by this user.
   * contractId: Optional. If set, the response will only include bets on this contract.
   * contractSlug: Optional. If set, the response will only include bets on this contract.
   * limit: Optional. How many bets to return. The maximum and the default are 1000.
   * before: Optional. The ID of the bet before which the list will start. For example, if you ask for the most recent 10 bets, and then perform a second query for 10 more bets with before=[the id of the 10th bet], you will get bets 11 through 20.
   */

//    def getBets(
//      userId: Option[String] = None,
//        username: Option[String] = None,
//        contractId: Option[String] = None,
//        contractSlug: Option[String] = None,
//        limit: Option[Int] = None,
//        before: Option[String] = None,
//               ): Either[String, List[Bet]] = {
//        val options = List(
//            userId.map(u => s"userId=$u"),
//            username.map(u => s"username=$u"),
//            contractId.map(c => s"contractId=$c"),
//            contractSlug.map(c => s"contractSlug=$c"),
//            limit.map(l => s"limit=$l"),
//            before.map(b => s"before=$b"),
//            ).flatten.mkString("?", "&", "")
//
//        val url = s"$baseUrl/bets$options"
//
//        val response = requests.get(url)
//
//        response.statusCode match {
//          case 200 =>
//            val bets = response.text()
//            bets.asJson.as[List[Bet]].left.map(_.toString)
//          case _ =>
//            Left(response.statusMessage)
//        }
//    }

  /**
   * @param order Either "shares" or "profit", default "profit"
   */
  def getMarketPositions(
    marketId: String,
    order: Option[String] = None,
    top: Option[Int] = None,
    bottom: Option[Int] = None,
    userId: Option[String] = None,
  ): Either[String, List[Position]] = {
    val options = List(
      order.map(o => s"order=$o"),
      top.map(t => s"top=$t"),
      bottom.map(b => s"bottom=$b"),
      userId.map(u => s"userId=$u"),
    ).flatten.mkString("?", "&", "")

    val url = s"$baseUrl/market/$marketId/positions$options"

    val response = requests.get(url)

    response.statusCode match {
      case 200 =>
        val positions = response.text()
        decode[List[Position]](positions).left.map(_.toString)
      case _ =>
        Left(response.statusMessage)
    }
  }
}
