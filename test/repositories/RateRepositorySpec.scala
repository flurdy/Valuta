package repositories

import com.redis._
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

   def given = afterWord("given")

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

      val redisProvider = new DefaultRedisProvider(databaseConfig = dbConfig)
      val rateRepository = new DefaultRateRepository(redisProvider = redisProvider)

      def findLatestRate(currency: Currency, pairCurrency: Currency): Future[Option[BigDecimal]] =
         rateRepository.findCurrencyPairRate(currency, pairCurrency)

   }

   "RateRepository" should {
      "save CryptoPerCryptoRate" in new Setup {
         val rate = CryptoPerCryptoRate(
            CryptoCurrency.XRP, CryptoCurrency.BTC, BigDecimal("0.0024"))

         whenReady(findLatestRate(CryptoCurrency.XRP, CryptoCurrency.BTC)){ rateBefore =>
            whenReady(rateRepository.saveCryptoPerCryptoRate(rate)){ _ =>
               whenReady(findLatestRate(CryptoCurrency.XRP, CryptoCurrency.BTC)){ rateAfter =>


                  assert(rateBefore == None)
                  assert(rateAfter == Some(BigDecimal("0.0024")))
               }
            }
         }
      }
      "save CryptoPerFiatRate" in new Setup {
         val rate = CryptoPerFiatRate(
            CryptoCurrency.BTC, FiatCurrency.USD, BigDecimal("14500"))

         whenReady(findLatestRate(CryptoCurrency.BTC, FiatCurrency.USD)){ rateBefore =>
            whenReady(rateRepository.saveCryptoPerFiatRate(rate)){ _ =>
               whenReady(findLatestRate(CryptoCurrency.BTC, FiatCurrency.USD)){ rateAfter =>

                  assert(rateBefore == None)
                  assert(rateAfter == Some(BigDecimal("14500")))
               }
            }
         }
      }
      "save FiatPerFiatRate" in new Setup {
         val rate = FiatPerFiatRate(
            FiatCurrency.USD, FiatCurrency.GBP, BigDecimal("14500"))

         whenReady(findLatestRate(FiatCurrency.USD, FiatCurrency.GBP)){ rateBefore =>
            whenReady(rateRepository.saveFiatPerFiatRate(rate)){ _ =>
               whenReady(findLatestRate(FiatCurrency.USD, FiatCurrency.GBP)){ rateAfter =>

                  assert(rateBefore == None)
                  assert(rateAfter == Some(BigDecimal("14500")))
               }
            }
         }
      }
      "save multiple CryptoPerFiat rates" in new Setup {
         val rate1 = CryptoPerFiatRate(
            CryptoCurrency.ETH, FiatCurrency.USD, BigDecimal("955"))
         val rate2 = CryptoPerFiatRate(
            CryptoCurrency.ETH, FiatCurrency.USD, BigDecimal("966.23"))

         whenReady(rateRepository.saveCryptoPerFiatRate(rate1)){ _ =>
            whenReady(findLatestRate(CryptoCurrency.ETH, FiatCurrency.USD)){ rateAfter1 =>
               whenReady(rateRepository.saveCryptoPerFiatRate(rate2)){ _ =>
                  whenReady(findLatestRate(CryptoCurrency.ETH, FiatCurrency.USD)){ rateAfter2 =>

                     assert(rateAfter1 == Some(BigDecimal("955")))
                     assert(rateAfter2 == Some(BigDecimal("966.23")))
                  }
               }
            }
         }
      }
   }

}
