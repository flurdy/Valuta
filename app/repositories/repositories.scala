package repositories

import com.google.inject.ImplementedBy
import com.redis._
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import models._
import CryptoCurrency._
import FiatCurrency._


@ImplementedBy(classOf[DefaultRateRepository])
trait RateWriteRepository extends WithLogger {

   def redisClients: RedisClientPool

   def saveCryptoPerCryptoRate(rate: CryptoPerCryptoRate): Future[Unit] =
      Future.failed(new RuntimeException("!!!"))

   def saveCryptoPerFiatRate(rate: CryptoPerFiatRate)(implicit ec: ExecutionContext): Future[Unit] = {
      redisClients.withClient { client =>

         val date = "1970-01-01"

         // todo date list
         Future {

            // if(!client.sismember("rates.pairs",s"${rate.dividen.value}-${rate.divisor.value}"))
               client.sadd("rates.pairs",s"${rate.dividen.value}-${rate.divisor.value}")
            // if(!client.sismember(s"rates.pair.${rate.dividen.value}-${rate.divisor.value}.dates", date))
               client.sadd(s"rates.pair.${rate.dividen.value}-${rate.divisor.value}.dates", date)
            client.set(s"rates.pair.${rate.dividen.value}-${rate.divisor.value}.${date}.rate", rate.rate)
         }
         // Future.failed(new RuntimeException("!!!"))
      }
   }

   def saveFiatPerFiatRate(rate: FiatPerFiatRate): Future[Unit] =
      Future.failed(new RuntimeException("!!!"))
}

@ImplementedBy(classOf[DefaultRateRepository])
trait RateReadRepository extends WithLogger {

   def findCryptoCurrencies()(implicit ec: ExecutionContext): Future[List[CryptoCurrency]] =
      Future {
         List(
            BTC, BCH, ETH, LTC, XRP, ADA, IOTA
         )
      }

   def findCurrencyPairRate(currency: Currency, pairCurrency: Currency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      currency match {
         case c: CryptoCurrency => findCryptoPairRate(c, pairCurrency)
         case c: FiatCurrency => findFiatPairRate(c, pairCurrency)
      }


   def findCryptoPairRate(currency: CryptoCurrency, pairCurrency: Currency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      Future {
         logger.info(s"Looking for stored crypto $currency")
         None
      }

   def findFiatPairRate(currency: FiatCurrency, pairCurrency: Currency)(implicit ec: ExecutionContext): Future[Option[BigDecimal]] =
      Future {
         logger.info(s"Looking for stored fiat $currency")
         None
      }

}

trait RateRepository extends RateWriteRepository with RateReadRepository

@Singleton
class DefaultRateRepository @Inject() (val databaseConfig: DatabaseConfiguration) extends RateRepository {
   val redisClients = new RedisClientPool(databaseConfig.redisHost, databaseConfig.redisPort)
}
