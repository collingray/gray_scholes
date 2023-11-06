package gray_scholes

import java.time.Instant
import scala.concurrent.duration.{DurationInt, FiniteDuration}

case class TimedCacheManager[K, V](
  getFunc: K => Option[V],
  validDuration: FiniteDuration = 1.day,
) {

  private var cache: Map[K, (Instant, V)] = Map.empty[K, (Instant, V)]

  def get(key: K): Option[V] = {
    cache.get(key) match {
      case Some((timeFetched, iv)) =>
        if (timeFetched.plusMillis(validDuration.toMillis).isAfter(Instant.now())) {
          Some(iv)
        } else {
          cache = cache - key
          get(key)
        }
      case None => {
        val value = getFunc(key)
        value.foreach(value => cache = cache + (key -> (Instant.now(), value)))
        value
      }
    }
  }
}
