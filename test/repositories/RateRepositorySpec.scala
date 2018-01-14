package repositories

import akka.actor.ActorSystem
import java.time.LocalDateTime
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play._
import play.api.Configuration
import redis.embedded.RedisServer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.Future
import scala.util.Random
import models._


trait MockRedisServer {

   val randomPort = 23000 + Random.nextInt(3000)
   lazy val redisServer = new RedisServer(randomPort)

   def startRedis(){
      redisServer.start()
   }

   def stopRedis(){
      redisServer.stop()
   }

   def redisConfiguration =
      Map(
         "database.redis.host" -> "127.0.0.1",
         "database.redis.port" -> redisServer.ports().get(0)
      )
}


class RateRepositoryComponentSpec extends PlaySpec with ScalaFutures with IntegrationPatience
with BeforeAndAfterAll with MockRedisServer {

   override def beforeAll() {
      startRedis()
      super.beforeAll()
   }

   override def afterAll() {
      stopRedis()
      super.afterAll()
   }
   trait Setup {

      val dbConfig = new DatabaseConfiguration{
         val rootConfig = Configuration.from(redisConfiguration)
      }

      val akkaSystem = ActorSystem("MySpec")
      val redisProvider = new DefaultRedisProvider(databaseConfig = dbConfig)(akkaSystem)
      val rateRepository = new DefaultRateRepository(redisProvider = redisProvider)

      def findLatestRate(dividen: Currency, divisor: Currency) =
         rateRepository.findCurrencyRate(dividen, divisor)

   }

   "RateRepository" should {
      "save CurrencyRate" in new Setup {

         val dividen = Currency.BTC
         val divisor = Currency.USD
         val date = LocalDateTime.now.minusMonths(5)
         val rate = BigDecimal("24201.12")
         val source = Some(RateSource.Bitstamp)
         val currencyRate = CurrencyRate(dividen, divisor, date, rate, source)

         val flow =
            for {
               rateBefore <- findLatestRate(Currency.BTC, Currency.USD)
               _          <- rateRepository.saveCurrencyRate(currencyRate)
               rateAfter  <- findLatestRate(Currency.BTC, Currency.USD)
            } yield (rateBefore, rateAfter)

         whenReady(flow){ case (rateBefore, rateAfter) =>

            assert(rateBefore == None)
            assert(rateAfter.map(_.rate) == Some(BigDecimal("24201.12")))
         }
      }

      "save multiple CurrencyRates" in new Setup {

         val dividen = Currency.ETH
         val divisor = Currency.USD
         val source = Some(RateSource.Bitstamp)
         val date1 = LocalDateTime.now.minusMonths(2)
         val date2 = LocalDateTime.now.minusMonths(1)
         val rate1 = BigDecimal("14201.12")
         val rate2 = BigDecimal("42101.12")
         val currencyRate1 = CurrencyRate(dividen, divisor, date1, rate1, source)
         val currencyRate2 = CurrencyRate(dividen, divisor, date2, rate2, source)

         val flow =
            for {
               _          <- rateRepository.saveCurrencyRate(currencyRate1)
               rateDuring <- findLatestRate(Currency.ETH, Currency.USD)
               _          <- rateRepository.saveCurrencyRate(currencyRate2)
               rateAfter  <- findLatestRate(Currency.ETH, Currency.USD)
            } yield (rateDuring, rateAfter)

         whenReady(flow){ case (rateDuring, rateAfter) =>

            assert(rateDuring.map(_.rate) == Some(BigDecimal("14201.12")))
            assert(rateAfter.map(_.rate)  == Some(BigDecimal("42101.12")))
         }

      }
   }

}
