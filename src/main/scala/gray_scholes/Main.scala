package gray_scholes

import gray_scholes.types.Market
import gray_scholes.utils.{getManifoldOption, trackableMarket}

object Main extends App {
  println("Hello world")

  val stockGroupId = "QDQfgsFiQrNNlZhsRGf5"

  val api = ManifoldAPI()

  val initialMarkets = api.getMarkets(limit = Some(100), groupId = Some(stockGroupId)).toOption.get

  assert(initialMarkets.nonEmpty, "Issue getting initial markets")

  // todo: remove limit when not testing
  // todo: cache these
  var markets: List[Market] = initialMarkets

  var newestMarketId: String = initialMarkets.head.id

  // map from market id to its implicit option
  var manifoldOptions: Map[String, ManifoldOption] = Map.empty[String, ManifoldOption]


  val priceCache = TimedCacheManager(utils.getAssetPrice)
  val ivCache = TimedCacheManager(utils.getImpliedVolatility)

  while (true) {
    // update states of previously found markets
    markets = markets.flatMap(market => api.getMarket(market.id).toOption)

    // get new markets
    val newMarkets = api.getMarketsSince(after = newestMarketId, groupId = Some(stockGroupId)).getOrElse(List.empty)
    markets = markets ++ newMarkets
    if (newMarkets.nonEmpty) {
      newestMarketId = newMarkets.head.id
      println(s"${newMarkets.length} new market(s) found")
    }

    // filter all markets
    markets = markets.filter(trackableMarket)

    // get manifold options for all markets that don't currently have them
    // lol i don't update these, ripe for exploiting by just changing the expiration date/ticker etc.
    manifoldOptions = manifoldOptions ++ markets
      .filterNot(market => manifoldOptions.contains(market.id))
      .flatMap(market => getManifoldOption(market).map(market.id -> _))
      .toMap

    // remove all markets that we couldn't get options for
    markets = markets.filter(market => manifoldOptions.contains(market.id))

    // calculate likelihoods of each market
    val marketLikelihoods = markets.flatMap { market =>
      val option = manifoldOptions(market.id)

      for {
        price <- priceCache.get(option.ticker)
        iv <- ivCache.get(option.ticker)
      } yield {
        val likelihood = utils.calculateLikelihood(option, price, iv)
        market.id -> likelihood
      }
    }

    // act on decisions
    // todo: for now we just print everything

    Thread.sleep(Parameters.PRIMARY_PERIOD)
  }
}
