import io.circe.parser.decode
import io.circe.syntax._
import io.circe.{Decoder, Encoder, HCursor, Json}
import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers.convertToAnyShouldWrapper
import gray_scholes.types._

class ManifoldTypesSpec extends AnyFlatSpec {
  val market1: String =
    """
        |{
        |  "id": "th5wnYl6yKJlezWZo8Na",
        |  "creatorId": "diILsiFywmhSZTzMgDpP1tlKaRJ3",
        |  "creatorUsername": "AaronBailey",
        |  "creatorName": "Aaron Bailey",
        |  "createdTime": 1698094615451,
        |  "creatorAvatarUrl": "https://firebasestorage.googleapis.com/v0/b/mantic-markets.appspot.com/o/user-images%2FAaronBailey%2FZaRjalaE3k.jpeg?alt=media&token=f0d88c46-2b41-4cc0-889e-13154d942b0f",
        |  "closeTime": 1698735540000,
        |  "question": "Are reports accurate that Putin had a cardiac arrest event?",
        |  "slug": "are-reports-accurate-that-putin-had",
        |  "url": "https://manifold.markets/AaronBailey/are-reports-accurate-that-putin-had",
        |  "pool": {
        |    "NO": 50,
        |    "YES": 50
        |  },
        |  "probability": 0.5,
        |  "p": 0.5,
        |  "totalLiquidity": 50,
        |  "outcomeType": "BINARY",
        |  "mechanism": "cpmm-1",
        |  "volume": 0,
        |  "volume24Hours": 0,
        |  "isResolved": false,
        |  "lastUpdatedTime": 1698094615451
        |}
        |""".stripMargin

  val market2: String =
    """
        |{
        |  "id":"YSGANpRXIC5HK37Tc4Pw",
        |  "creatorId":"D1SxDsn7jrco9kYhE9C3Rldi7qw1",
        |  "creatorUsername":"logaems",
        |  "creatorName":"Parvati Jain",
        |  "createdTime":1698965262693,
        |  "creatorAvatarUrl":"https://firebasestorage.googleapis.com/v0/b/mantic-markets.appspot.com/o/user-images%2FLogaems%2Flove-images%2Fmrjjwft5DS.37?alt=media&token=f563a08f-6e8a-40cb-b796-89cad49e1476",
        |  "closeTime":1701557940000,
        |  "question":"Will I finish at a higher place than Eliezer Yudkowsky?",
        |  "slug":"will-i-finish-at-a-higher-place-tha",
        |  "url":"https://manifold.markets/logaems/will-i-finish-at-a-higher-place-tha",
        |  "pool":{"NO":53.095000460982476,"YES":53.09500046098249},
        |  "probability":0.5000000000000001,
        |  "p":0.5000000000000002,
        |  "totalLiquidity":70,
        |  "outcomeType":"BINARY",
        |  "mechanism":"cpmm-1",
        |  "volume":19.999999999999996,
        |  "volume24Hours":0,
        |  "isResolved":false,
        |  "lastUpdatedTime":1698965919386,
        |  "description":{"type":"doc","content":[{"type":"paragraph","content":[{"text":"Will I be at a higher place in the league \"Serendipitious Wraiths\", when season 7 closes?","type":"text"}]}]},"coverImageUrl":"https://firebasestorage.googleapis.com/v0/b/mantic-markets.appspot.com/o/dream%2FvMXrLcZs50.png?alt=media&token=01d9dfda-508a-462f-95fa-d575b7115fb0","groupSlugs":["eliezer-yudkowsky","eliezer-yudkowsky-25acf68e45f1","eliezer-yudkowsky-cec5f69cbdd8"],"textDescription":"Will I be at a higher place in the league \"Serendipitious Wraiths\", when season 7 closes?"
        |}
        |""".stripMargin
  val position: String =
    """
        |{
        |  "from": {
        |    "day": {
        |      "value": 23.479030029570662,
        |      "profit": 0,
        |      "invested": 23.479030029570662,
        |      "prevValue": 23.479030029570662,
        |      "profitPercent": 0
        |    },
        |    "week": {
        |      "value": 0,
        |      "profit": 8.479030029570673,
        |      "invested": 15,
        |      "prevValue": 0,
        |      "profitPercent": 56.52686686380448
        |    },
        |    "month": {
        |      "value": 0,
        |      "profit": 8.479030029570673,
        |      "invested": 15,
        |      "prevValue": 0,
        |      "profitPercent": 56.52686686380448
        |    }
        |  },
        |  "loan": 1.7123642870400002,
        |  "payout": 23.479030029570673,
        |  "profit": 8.479030029570673,
        |  "userId": "IpTiwOTs96VIzeoxu66tfitUcBZ2",
        |  "invested": 15,
        |  "userName": "Lucas Goldfein",
        |  "hasShares": true,
        |  "contractId": "kupKInoLsjMuiDiNfogm",
        |  "hasNoShares": true,
        |  "lastBetTime": 1678924706057,
        |  "totalShares": {
        |    "NO": 89.17418492518308
        |  },
        |  "hasYesShares": false,
        |  "userUsername": "LucasGoldfein56b1",
        |  "profitPercent": 56.52686686380448,
        |  "userAvatarUrl": "https://lh3.googleusercontent.com/a/AEdFTp5e7cFzq1moc91CKqaAgyEleoNTjtEL9ke8emzV=s96-c",
        |  "maxSharesOutcome": "NO"
        |}
        |""".stripMargin

  "market 1" should "decode" in {
    val decoded = decode[Market](market1)
    println(decoded)
    decoded.isRight shouldBe true
  }

  "market 2" should "decode" in {
    val decoded = decode[Market](market2)
    println(decoded)
    decoded.isRight shouldBe true
  }

  "position" should "decode" in {
    val decoded = decode[Position](position)
    println(decoded)
    decoded.isRight shouldBe true
  }
}
