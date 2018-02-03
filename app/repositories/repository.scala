package repositories

import akka.actor.ActorSystem
import akka.util.ByteString
import com.google.inject.ImplementedBy
import redis.RedisClient
import java.time.{LocalDateTime, ZoneOffset}
import java.time.format.DateTimeFormatter
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import com.flurdy.sander.primitives._
import models._


@ImplementedBy(classOf[DefaultRedisProvider])
trait RedisProvider {
   def client: RedisClient
}


@Singleton
class DefaultRedisProvider @Inject()
(val databaseConfig: DatabaseConfiguration)(implicit akkaSystem: ActorSystem)
extends RedisProvider with WithLogger {

   // logger.info(s"Connecting Redis client to: ${databaseConfig.redisHost}:${databaseConfig.redisPort}")

   lazy val client = {
      logger.error("####### REDIS URL" + databaseConfig.redisUrl)
      logger.error("####### REDIS HOST" + databaseConfig.redisHost)
      logger.error("####### REDIS PORT" + databaseConfig.redisPort.toInt )
      new RedisClient(port = databaseConfig.redisPort.toInt )
   }

}


trait CryptoKeys {

   val rootKey       = "valuta:rates"
   val currenciesKey = "valuta:rates:currencies"
   def dividenRootKey(dividen: Currency)    = s"valuta:rates:currency:${dividen}"
   def dividenPairsKey(dividen: Currency)   = s"valuta:rates:currency:${dividen}:pairs"
   def currencyDatesKey(dividen: Currency)  = s"valuta:rates:currency:${dividen}:dates"
   def currencyPairRootKey(pair: RatePair)  = s"valuta:rates:currency:${pair.dividen}:pair:${pair.divisor}"
   def currencyPairDatesKey(pair: RatePair) = s"valuta:rates:currency:${pair.dividen}:pair:${pair.divisor}:dates"
   def currencyPairRatesKey(pair: RatePair) = s"valuta:rates:currency:${pair.dividen}:pair:${pair.divisor}:rates"

}


@ImplementedBy(classOf[DefaultRateRepository])
trait RateWriteRepository extends CryptoKeys with WithLogger {

   def redisProvider: RedisProvider

   val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

   def saveCurrencyRate(rate: CurrencyRate)(implicit ec: ExecutionContext) = {

      logger.info(s"Saving rate for ${rate.pair}")

      def addDividen(): Future[Unit] =
            redisProvider.client.sadd(currenciesKey, rate.pair.dividen.entryName) map {
               case 1L =>
                  logger.info(s"Adding ${rate.pair.dividen} to $currenciesKey" )
               case _ => ()
                  // logger.debug(s"Not adding ${rate.pair.dividen} to $currenciesKey" )
            }

      def addDivisorPair(): Future[Unit] =
            redisProvider.client.sadd(dividenPairsKey(rate.pair.dividen), rate.pair.divisor.entryName) map {
               case 1L =>
                  logger.info(s"Adding ${rate.pair.divisor} to ${dividenPairsKey(rate.pair.dividen)}")
               case _ => ()
               //    logger.debug(s"Not adding ${rate.pair.divisor} to ${dividenPairsKey(rate.pair.dividen)}")
            }

      def addDividenDate(): Future[Unit] =
            redisProvider.client.zadd(currencyDatesKey(rate.pair.dividen),
                                     (rate.epochSecond.toDouble, rate.epochSecond.toString)) map {
                case 1L =>
                  logger.info(s"Adding ${rate.date} ${rate.epochSecond} to ${currencyDatesKey(rate.pair.dividen)}")
                case _ => ()
                  // logger.info(s"Not adding ${rate.epochSecond} ${rate.epochSecond} to ${currencyDatesKey(rate.pair.dividen)}")
             }

      def addDivisorDate(): Future[Unit] =
            redisProvider.client.zadd(currencyPairDatesKey(rate.pair),
                                     (rate.epochSecond.toDouble, rate.epochSecond.toString)) map {
                case 1L =>
                  logger.info(s"Adding ${rate.date} ${rate.epochSecond} to ${currencyPairDatesKey(rate.pair)}")
                case _ => ()
                  // logger.debug(s"No need to add ${rate.epochSecond} ${rate.epochSecond} to ${currencyPairDatesKey(rate.pair)}")
             }

      def addRate(): Future[Unit] =
            redisProvider.client.hset(currencyPairRatesKey(rate.pair),
                                     rate.epochSecond.toString, rate.rate.toString) map {
               case true  =>
                  logger.info(s"Adding ${rate.rate} with ${rate.epochSecond} to ${currencyPairRatesKey(rate.pair)}")
               case false =>
                   logger.info(s"Overwrote ${rate.rate} with ${rate.epochSecond} to ${currencyPairRatesKey(rate.pair)}")
            }

      for {
         _ <- addDividen()
         _ <- addDivisorPair()
         _ <- addDividenDate()
         _ <- addDivisorDate()
         _ <- addRate()
      } yield rate
   }

}


@ImplementedBy(classOf[DefaultRateRepository])
trait RateReadRepository extends CryptoKeys with WithLogger {

   def redisProvider: RedisProvider

   private def toDate(date: String) = LocalDateTime.ofEpochSecond(date.toLong, 0, ZoneOffset.UTC)

   private def toCurrency(currency: ByteString) = Currency.withNameOption(currency.utf8String)

   private def findDateRate(pair: RatePair, date: String)(implicit ec: ExecutionContext):
         Future[Option[CurrencyRate]] =
       redisProvider.client.hget(currencyPairRatesKey(pair), date)
               .map( r1 => r1.map( r2 => CurrencyRate( pair, toDate(date), BigDecimal(r2.utf8String), None, Some(SourcedFrom.FromCache))))

   def findCurrencyRate(pair: RatePair)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]] = {

      def findLatestDate(isKnownDividen: Boolean, isKnownDivisor: Boolean): Future[Option[String]] =
         if(isKnownDividen && isKnownDivisor)
               redisProvider.client.zrevrange(currencyPairDatesKey(pair), 0, 0)
                  .map{ _.headOption.map(_.utf8String) }
         else Future.successful(None)

      def findRate(latestDate: Option[String]): Future[Option[CurrencyRate]] =
            latestDate.fold[Future[Option[CurrencyRate]]]{
               Future.successful(None)
            }{ date =>
               findDateRate(pair, date)
            }

      for {
            isKnownDividen <- isKnownDividenCurrency(pair.dividen)
            isKnownDivisor <- isKnownPair(pair)
            latestDate     <- findLatestDate(isKnownDividen, isKnownDivisor)
            currencyRate   <- findRate(latestDate)
         } yield currencyRate
   }

   private def findKnownDividens()(implicit ec: ExecutionContext): Future[List[Currency]] = {
      redisProvider.client.smembers(currenciesKey) map { members =>
         members.map( toCurrency(_) )
                .toList
                .flatten
      }
   }

   private def isKnownDividenCurrency(dividen: Currency): Future[Boolean] =
      redisProvider.client.sismember(currenciesKey, dividen.entryName)

   private def isKnownPair(pair: RatePair): Future[Boolean] =
         redisProvider.client.sismember(dividenPairsKey(pair.dividen), pair.divisor.entryName)

   def findDivisorsForDividen(dividen: Currency)(implicit ec: ExecutionContext): Future[List[Currency]] =
      isKnownDividenCurrency(dividen) flatMap {
         case true =>
            redisProvider.client.smembers(dividenPairsKey(dividen))
               .map{ members =>
                     members.map( toCurrency(_) )
                            .toList
                            .flatten
               }
         case false => Future.successful(List.empty)
      }

   def findDividensForDivisor(divisor: Currency)(implicit ec: ExecutionContext): Future[List[Currency]] =
      findKnownDividens().map { dividens =>
         dividens.map { dividen =>
            isKnownPair(RatePair(dividen, divisor)) map {
               (_, dividen)
            }
         }
      }.map ( Future.sequence(_) )
      .flatten
      .map { knownDividens =>
         knownDividens.filter( _._1 )
                      .map( _._2 )
      }

   def findRatesByDates(dividen: Currency)(implicit ec: ExecutionContext): Future[DateRates] = {

      def findDatesForCurrency(): Future[List[String]] =
         redisProvider.client.zrevrange(currencyDatesKey(dividen), 0, 10)
            .map(
               _.toList
                .map(_.utf8String)
            )

      def findDivisorRates(date: String): Future[DivisorRates] =
         findDivisorsForDividen(dividen) flatMap { divisors =>
            (Future.sequence {
               divisors map { divisor =>
                  findDateRate( RatePair(dividen, divisor), date)
                     .map( rate => rate map ( r => (divisor,r) ) )
               }
            }).map( _.filter( _.isDefined )
                     .map( _.get )
                     .toMap )
              .map( DivisorRates(_) )
         }

      isKnownDividenCurrency(dividen) flatMap {
         case true =>
            findDatesForCurrency() flatMap { dates =>
               (Future.sequence {
                  dates map { date =>
                     findDivisorRates(date) map { divisorRates =>
                        (toDate(date), divisorRates)
                     }
                  }
               }).map( _.toMap )
                 .map( DateRates(_) )
            }
         case false =>
            Future.successful(DateRates(Map.empty))
      }
   }
}


@ImplementedBy(classOf[DefaultRateRepository])
trait RateRepository extends RateWriteRepository with RateReadRepository


@Singleton
class DefaultRateRepository @Inject() (val redisProvider: RedisProvider) extends RateRepository
