package gray_scholes

import gray_scholes.types.Market

import java.time.{Instant, ZonedDateTime}
import java.util.Date
import ujson._

import scala.util.Try

object utils {
  private val polygonApiKey = sys.env("POLYGON_API_KEY")
  private val openaiApiKey = sys.env("OPENAI_API_KEY")


  def trackableMarket(market: Market): Boolean = {
    //todo: add more, maybe liquidity check
    !market.isResolved
  }

  def getManifoldOption(market: Market): Option[ManifoldOption] = {
    val now = ZonedDateTime.now.withZoneSameInstant(java.time.ZoneId.of("America/New_York"))

    val prompt =
      s"""
           |You will be provided with a question, and your task is not to answer it, but to provide information about it.
           |
           |Please reply with the following information, extracted from the provided description:
           |style: American or European
           |strike: The strike price of the option
           |ticker: The ticker of the underlying asset
           |expiration: The expiration date of the option in the format MM/DD/YYYY
           |
           |The style is American if the question asks whether the price will be reached at any point during the period,
           |and European if the question asks whether it will be at some price on the expiration date.
           |
           |Format your response as a json object with the following fields:
           |{
           |  "style": "American"|"European",
           |  "strike": number,
           |  "ticker": string,
           |  "expiration": string,
           |}
           |
           |The current date is ${now.getMonthValue}/${now.getDayOfMonth}/${now.getYear}.
           |The current time is ${now.getHour}:${now.getMinute}:${now.getSecond}.
           |
           |""".stripMargin

    val headers = Map(
      "Authorization" -> s"Bearer $openaiApiKey",
      "Content-Type" -> "application/json",
    )

    val data = (
      Obj(
        "messages" -> Arr(
          Obj(
            "role" -> "system",
            "content" -> prompt,
          ),
          Obj(
            "role" -> "user",
            "content" -> market.question,
          ),
        ),
        "model" -> "gpt-3.5-turbo",
      ),
    ).render()

    // call the openai api
    val reply = requests.post(
      "https://api.openai.com/v1/chat/completions",
      headers = headers,
      data = data,
    )

    println(reply.text)

    // parse the response
    val res = for {
      json <- io.circe.parser.parse(reply.text)
      obj <- json.asObject.toRight("Failed to parse json")
      choices <- obj("choices").toRight("Failed to get choices")
      choice <- choices.asArray.flatMap(_.headOption).toRight("Failed to get first choice")
      choiceObj <- choice.asObject.toRight("Failed to get choice object")
      message <- choiceObj("message").toRight("Failed to get message")
      messageObj <- message.asObject.toRight("Failed to get message object")
      content <- messageObj("content").toRight("Failed to get content")
      _ = println(content.toString)
      cleanedContentString =
        content.toString
          .replace("\\n", "\n")
          .replace("\\", "")
          .drop(1)
          .dropRight(1)
      _ = println(cleanedContentString)
      contentJson <- io.circe.parser.parse(cleanedContentString)
      contentObj <- contentJson.asObject.toRight("Failed to get content object")
      style <- contentObj("style").toRight("Failed to get style")
      styleStr <- style.asString.toRight("Failed to get style string")
      style <- styleStr match {
        case "American" => Right(American)
        case "European" => Right(European)
        case _          => Left("Invalid style")
      }
      strike <- contentObj("strike").toRight("Failed to get strike")
      strikeNum <- strike.asNumber.map(_.toDouble).toRight("Failed to get strike number")
      ticker <- contentObj("ticker").toRight("Failed to get ticker")
      tickerStr <- ticker.asString.toRight("Failed to get ticker string")
      expiration <- contentObj("expiration").toRight("Failed to get expiration")
      expirationStr <- expiration.asString.toRight("Failed to get expiration string")
      expiration <- scala.util
        .Try(Instant.ofEpochMilli(Date.parse(expirationStr)))
        .toOption
        .toRight("Failed to parse expiration")
    } yield ManifoldOption(
      style = style,
      strike = strikeNum,
      ticker = tickerStr,
      expiration = expiration,
    )

    res match {
      case Left(err)     => println(err); None
      case Right(option) => Some(option)
    }
  }

  // Uses polygon.io to grab the average implied volatility across the option chain of the given ticker
  def getImpliedVolatility(ticker: String): Option[Double] = {
    var endpoint: Option[String] = Some(s"https://api.polygon.io/v3/snapshot/options/$ticker?limit=250")

    val headers = Map(
      "Authorization" -> s"Bearer $polygonApiKey"
    )

    // call the endpoint, while next_url is set keep calling
    var replies: List[String] = List.empty

    while (endpoint.isDefined) {
      val reply = requests.get(
        endpoint.get,
        headers = headers,
      )

      val replyText = reply.text

      replies = replyText :: replies

      val json = read(Readable.fromString(replyText))

      endpoint = Try(json("next_url").str).toOption
    }

    val resultJsons = replies
      .flatMap { reply =>
        val json = ujson.read(reply)

        Try(json("results").arr).toOption
      }
      .flatten
      // filter to only options with implied_volatility and day volume fields present
      .filter { resultJson =>
        Try(resultJson("implied_volatility").num).isSuccess &&
        Try(resultJson("day")("volume").num).isSuccess
      }

    if (resultJsons.isEmpty) {
      return None
    }

    // average of implied_volatility, weighted by day volume
    val volumeImpliedVolTuple = resultJsons
      .map { resultJson =>
        val volume = resultJson("day")("volume").num
        val impliedVolatility = resultJson("implied_volatility").num

        (volume, impliedVolatility)
      }

    val totalVolume = volumeImpliedVolTuple.map(_._1).sum

    val weightedAverage = volumeImpliedVolTuple.map {
      case (volume, impliedVolatility) =>
        volume / totalVolume * impliedVolatility
    }.sum

    Some(weightedAverage)
  }

  def getAssetPrice(ticker: String): Option[Double] = {
    val endpoint = s"https://api.polygon.io/v2/snapshot/locale/us/markets/stocks/tickers/$ticker"

    val headers = Map(
      "Authorization" -> s"Bearer $polygonApiKey"
    )

    val reply = requests.get(
      endpoint,
      headers = headers,
    )

    val replyText = reply.text

    val json = ujson.read(replyText)

    Try(json("ticker")("day")("c").num).toOption
  }

  def tradingDaysBetween(t1: Instant, t2: Instant): Int = {
    val days = (t2.toEpochMilli - t1.toEpochMilli) / (1000 * 60 * 60 * 24)
    val weekends = days / 7 * 2
    val startDay = t1.atZone(java.time.ZoneId.of("America/New_York")).getDayOfWeek.getValue
    val endDay = t2.atZone(java.time.ZoneId.of("America/New_York")).getDayOfWeek.getValue
    val weekdays = (startDay to endDay).count(day => day != 6 && day != 7)
    weekdays - weekends
  }

  // Calculates likelihood of being in the money for the given option, current price, and implied volatility
  def calculateLikelihood(option: ManifoldOption, price: Double, iv: Double): Double = {
    val daysToExpiration = option.expiration
    val stdDevOverPeriod = daysToExpiration / 252
    // iv represents the annualized standard deviation
    // to go from it to the odds of being itm at expiration
    // 1. volatility over period = days(exp - now) / 252
    //
  }
}
