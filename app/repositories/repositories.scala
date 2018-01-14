package repositories

import akka.actor.ActorSystem
import akka.util.ByteString
import com.google.inject.{AbstractModule, ImplementedBy}
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

   logger.info(s"Connecting Redis client to: ${databaseConfig.redisHost}:${databaseConfig.redisPort}")

   lazy val client = new RedisClient(port = databaseConfig.redisPort.toInt )

}


trait CryptoKeys {

   val rootKey       = "valuta:rates"
   val currenciesKey = "valuta:rates:currencies"
   def dividenRootKey(dividen: Currency)                          = s"valuta:rates:currency:${dividen}"
   def dividenPairsKey(dividen: Currency)                         = s"valuta:rates:currency:${dividen}:pairs"
   def currencyDatesKey(dividen: Currency)                        = s"valuta:rates:currency:${dividen}:dates"
   def currencyPairRootKey(dividen: Currency,  divisor: Currency) = s"valuta:rates:currency:${dividen}:pair:${divisor}"
   def currencyPairDatesKey(dividen: Currency, divisor: Currency) = s"valuta:rates:currency:${dividen}:pair:${divisor}:dates"
   def currencyPairRatesKey(dividen: Currency, divisor: Currency) = s"valuta:rates:currency:${dividen}:pair:${divisor}:rates"

}


@ImplementedBy(classOf[DefaultRateRepository])
trait RateWriteRepository extends CryptoKeys with WithLogger {

   def redisProvider: RedisProvider

   val dateFormatter = DateTimeFormatter.ISO_LOCAL_DATE_TIME

   def saveCurrencyRate(rate: CurrencyRate)(implicit ec: ExecutionContext) = {

      def addDividen(): Future[Unit] =
            redisProvider.client.sadd(currenciesKey, rate.dividen.entryName) map {
               case 1L =>
                  logger.info(s"Adding ${rate.dividen} to $currenciesKey" )
               case _ =>
                  logger.debug(s"Not adding ${rate.dividen} to $currenciesKey" )
            }

      def addDivisorPair(): Future[Unit] =
            redisProvider.client.sadd(dividenPairsKey(rate.dividen), rate.divisor.entryName) map {
               case 1L =>
                  logger.info(s"Adding ${rate.divisor} to ${dividenPairsKey(rate.dividen)}")
               case _ =>
                  logger.debug(s"Not adding ${rate.divisor} to ${dividenPairsKey(rate.dividen)}")
            }

      def addDividenDate(): Future[Unit] =
            redisProvider.client.zadd(currencyDatesKey(rate.dividen),
                                     (rate.epochSecond.toDouble, rate.epochSecond.toString)) map {
                case 1L =>
                  logger.info(s"Adding ${rate.date} ${rate.epochSecond} to ${currencyDatesKey(rate.dividen)}")
                case _ =>
                  logger.info(s"Not adding ${rate.epochSecond} ${rate.epochSecond} to ${currencyDatesKey(rate.dividen)}")
             }

      def addDivisorDate(): Future[Unit] = {
            logger.debug("#### rate.date " + rate.date)
            logger.debug("#### rate.epochSecond " + rate.epochSecond)
            logger.debug("#### rate.epochSecond.toDouble " + rate.epochSecond.toDouble)
            redisProvider.client.zadd(currencyPairDatesKey(rate.dividen, rate.divisor),
                                     (rate.epochSecond.toDouble, rate.epochSecond.toString)) map {
                case 1L =>
                  logger.info(s"Adding ${rate.date} ${rate.epochSecond} to ${currencyPairDatesKey(rate.dividen, rate.divisor)}")
                case _ =>
                  logger.info(s"Not adding ${rate.epochSecond} ${rate.epochSecond} to ${currencyPairDatesKey(rate.dividen, rate.divisor)}")
             }
          }

      def addRate(): Future[Unit] =
            redisProvider.client.hset(currencyPairRatesKey(rate.dividen, rate.divisor),
                                     rate.epochSecond.toString, rate.rate.toString) map {
               case true  =>
                  logger.info(s"Adding ${rate.rate} with ${rate.epochSecond} to ${currencyPairRatesKey(rate.dividen, rate.divisor)}")
               case false =>
                   logger.info(s"Overwrote ${rate.rate} with ${rate.epochSecond} to ${currencyPairRatesKey(rate.dividen, rate.divisor)}")
            }

      for {
         _ <- addDividen()
         _ <- addDivisorPair()
         _ <- addDividenDate()
         _ <- addDivisorDate()
         _ <- addRate()
      } yield ()
   }

}


@ImplementedBy(classOf[DefaultRateRepository])
trait RateReadRepository extends CryptoKeys with WithLogger {

   def redisProvider: RedisProvider

   private def toDate(date: String) = LocalDateTime.ofEpochSecond(date.toLong, 0, ZoneOffset.UTC)

   private def findDateRate(dividen: Currency, divisor: Currency, date: String)(implicit ec: ExecutionContext):
         Future[Option[CurrencyRate]] =
       redisProvider.client.hget(currencyPairRatesKey(dividen, divisor), date)
               .map( r1 => r1.map( r2 => CurrencyRate(dividen, divisor, toDate(date), BigDecimal(r2.utf8String), None)))

   private def isKnownDividenCurrency(dividen: Currency): Future[Boolean] =
         redisProvider.client.sismember(currenciesKey, dividen.entryName)

   def findCurrencyRate(dividen: Currency, divisor: Currency)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]] = {

      def isKnownDivisorCurrency(): Future[Boolean] =
            redisProvider.client.sismember(dividenPairsKey(dividen), divisor.entryName)

      def findLatestDate(isKnownDividen: Boolean, isKnownDivisor: Boolean): Future[Option[String]] =
         if(isKnownDividen && isKnownDivisor)
               redisProvider.client.zrevrange(currencyPairDatesKey(dividen, divisor), 0, 0)
                  .map{ _.headOption.map(_.utf8String) }
         else Future.successful(None)

      def findRate(latestDate: Option[String]): Future[Option[CurrencyRate]] =
            latestDate.fold[Future[Option[CurrencyRate]]]{
               Future.successful(None)
            }{ date =>
               findDateRate(dividen, divisor, date)
            }

      for {
            isKnownDividen <- isKnownDividenCurrency(dividen)
            isKnownDivisor <- isKnownDivisorCurrency()
            latestDate     <- findLatestDate(isKnownDividen, isKnownDivisor)
            currencyRate   <- findRate(latestDate)
         } yield currencyRate
   }

   def findDivisorsForCurrency(dividen: Currency)(implicit ec: ExecutionContext): Future[List[Currency]] =
      isKnownDividenCurrency(dividen) flatMap {
         case true =>
            redisProvider.client.smembers(dividenPairsKey(dividen))
               .map{ divisors =>
                  divisors.map ( d => Currency.withNameOption(d.utf8String) )
                          .flatten
                          .toList
               }
         case false => Future.successful(List.empty)
      }

   def findRatesByDates(dividen: Currency)(implicit ec: ExecutionContext): Future[DateRates] =
      {

         def findDatesForCurrency(): Future[List[String]] =
            redisProvider.client.zrevrange(currencyDatesKey(dividen), 0, 10)
               .map(_.toList.map(_.utf8String))

         def findDivisorRates(date: String): Future[DivisorRates] =
            findDivisorsForCurrency(dividen) flatMap { divisors =>
               (Future.sequence {
                  divisors map { divisor =>
                     findDateRate( dividen, divisor, date)
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
