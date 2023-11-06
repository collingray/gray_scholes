import gray_scholes._
import gray_scholes.types.Market
import gray_scholes.types.Market.Mechanism
import org.scalatest.flatspec.AnyFlatSpec

import java.time.Instant
import java.util.Date

class UtilsSpec extends AnyFlatSpec {
  val market: Market = Market(
    id = "04gPhAvWNdvWAlaDJt2I",
    creatorUsername = "LightningBee",
    creatorName = "Lightning Bee ⚡ 🐝",
    createdTime = Instant.parse("2023-08-12T06:26:43.377Z"),
    creatorAvatarUrl = Some(value =
      "https://firebasestorage.googleapis.com/v0/b/mantic-markets.appspot.com/o/user-images%2FLightningBee%2F8SfMbbOv8w.jpg?alt=media&token=2e98dae2-0d31-4db9-8428-12a5273b4519"
    ),
    closeTime = Some(value = Instant.parse("2023-12-30T23:59:00Z")),
    question = "Will MicroStrategy (MSTR) stock price reach $500 in 2023?",
    url = "https://manifold.markets/LightningBee/will-microstrategy-mstr-stock-price",
    outcomeType = Market.OutcomeType.Binary,
    mechanism = Some(value = Mechanism.CPMM),
    probability = Some(value = 0.1044894341729344),
    volume = 530.3890254252988,
    volume24Hours = 0.0,
    isResolved = false,
    resolutionTime = None,
    resolution = None,
    resolutionProbability = None,
    lastUpdatedTime = Some(Instant.parse("2023-09-26T09:08:12.646Z")),
  )

  val expectedManifoldOption: ManifoldOption = ManifoldOption(
    style = American,
    strike = 500.0,
    ticker = "MSTR",
    expiration = Instant.ofEpochMilli(Date.parse("12/31/2023")),
  )

  "getManifoldOption" should "return the correct ManifoldOption" in {
    val actualManifoldOption = utils.getManifoldOption(market)
    assert(actualManifoldOption.contains(expectedManifoldOption))
  }
}
