package gray_scholes

object Parameters {
  val PRIMARY_PERIOD: Int = Option(System.getenv("PRIMARY_PERIOD")).flatMap(_.toIntOption).getOrElse(10000)
}
