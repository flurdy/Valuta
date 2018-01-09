package repositories

import com.google.inject.{AbstractModule, ImplementedBy}
import com.redis._
import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import javax.inject.{Inject, Singleton}
import play.api.inject.ApplicationLifecycle
import scala.concurrent.{ExecutionContext, Future}
import com.flurdy.sander.primitives._
import models._
import CryptoCurrency._
import FiatCurrency._


@ImplementedBy(classOf[DefaultRedisProvider])
trait RedisProvider {

   def pool: RedisClientPool

}

@Singleton
class DefaultRedisProvider @Inject() (val databaseConfig: DatabaseConfiguration) extends RedisProvider {

   lazy val pool = new RedisClientPool(databaseConfig.redisHost, databaseConfig.redisPort)

}


trait CryptoKeys {

   def rateDatesKey(dividen: String, divisor: String) =
      s"rates:pair:${dividen}-${divisor}:dates"

   def rateKey(dividen: String, divisor: String, date: String) =
       s"rates:pair:${dividen}-${divisor}.${date}:rate"

}


@ImplementedBy(classOf[DefaultRateRepository])
trait RateWriteRepository extends CryptoKeys with WithLogger {

   def redisProvider: RedisProvider

   val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

   private def saveRate(dividen: String, divisor: String, rate: BigDecimal)(implicit ec: ExecutionContext): Future[Unit] =
      redisProvider.pool.withClient { client =>
         val date = LocalDateTime.now
         Future {
            try {
               client.sadd("rates:pairs",s"${dividen}-${divisor}")
               client.zadd(rateDatesKey(dividen, divisor), date.toEpochSecond(ZoneOffset.UTC), dateFormatter.format(date))
               client.set(rateKey(dividen, divisor, date.toString), rate)
            } catch {
               case e: Exception =>
                  println(e)
                  throw e
               case e: Throwable =>
                  println(e)
                  throw new RuntimeException("whatevs")
            }
         }
      }

   def saveCryptoPerCryptoRate(rate: CryptoPerCryptoRate)(implicit ec: ExecutionContext) =
      saveRate(rate.dividen.value, rate.divisor.value, rate.rate)

   def saveCryptoPerFiatRate(rate: CryptoPerFiatRate)(implicit ec: ExecutionContext) =
      saveRate(rate.dividen.value, rate.divisor.value, rate.rate)

   def saveFiatPerFiatRate(rate: FiatPerFiatRate)(implicit ec: ExecutionContext) =
      saveRate(rate.dividen.value, rate.divisor.value, rate.rate)

}

@ImplementedBy(classOf[DefaultRateRepository])
trait RateReadRepository extends CryptoKeys with WithLogger {

   def redisProvider: RedisProvider

   def findCryptoCurrencies()(implicit ec: ExecutionContext): Future[List[CryptoCurrency]] =
      Future {
         List( BTC, BCH, ETH, LTC, XRP, ADA, IOTA )
      }

   def findCurrencyPairRate(dividen: Currency, divisor: Currency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      (dividen, divisor) match {
         case (c: CryptoCurrency, p: CryptoCurrency) => findCryptoPerCryptoPairRate(c, p)
         case (c: CryptoCurrency, p: FiatCurrency)   => findCryptoPerFiatPairRate(c, p)
         case (c: FiatCurrency,   p: FiatCurrency)   => findFiatPerFiatPairRate(c, p)
         case _ =>
            logger.debug(s"Fiat/Crypto not possible: $dividen / $divisor")
            Future.successful(None)
      }

   private def findCryptoPerCryptoPairRate(dividen: CryptoCurrency, divisor: CryptoCurrency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      redisProvider.pool.withClient { client =>
         logger.info(s"Looking for stored crypto $dividen/$divisor")
         Future {
            client.sismember("rates:pairs",s"${dividen.value}-${divisor.value}")
                  .some
                  .flatMap { _ =>
                     client.zrange(rateDatesKey(dividen.value, divisor.value), 0, 0, RedisClient.DESC)
                  }
                  .flatMap ( _.headOption )
                  .flatMap { date =>
                     client.get(rateKey(dividen.value, divisor.value, date))
                           .map(BigDecimal(_))
                  }
         }
      }

   private def findCryptoPerFiatPairRate(dividen: CryptoCurrency, divisor: FiatCurrency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      redisProvider.pool.withClient { client =>
         logger.info(s"Looking for stored crypto $dividen/$divisor")
         Future {
            client.sismember("rates:pairs",s"${dividen.value}-${divisor.value}")
                  .some
                  .flatMap { _ =>
                     client.zrange(rateDatesKey(dividen.value, divisor.value), 0, 0, RedisClient.DESC)
                  }
                  .flatMap ( _.headOption )
                  .flatMap { date =>
                     client.get(rateKey(dividen.value, divisor.value, date))
                           .map(BigDecimal(_))
                  }
         }
      }

   private def findFiatPerFiatPairRate(dividen: FiatCurrency, divisor: FiatCurrency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      redisProvider.pool.withClient { client =>
         logger.info(s"Looking for stored fiat $dividen")
         Future {
            client.sismember("rates:pairs",s"${dividen.value}-${divisor.value}")
                  .some
                  .flatMap { _ =>
                     client.zrange(rateDatesKey(dividen.value, divisor.value), 0, 0, RedisClient.DESC)
                  }
                  .flatMap ( _.headOption )
                  .flatMap { date =>
                     client.get(rateKey(dividen.value, divisor.value, date))
                           .map(BigDecimal(_))
                  }
         }
      }

}

@ImplementedBy(classOf[DefaultRateRepository])
trait RateRepository extends RateWriteRepository with RateReadRepository

@Singleton
class DefaultRateRepository @Inject() (val redisProvider: RedisProvider) extends RateRepository
