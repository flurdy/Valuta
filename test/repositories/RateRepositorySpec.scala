package repositories

import ai.grakn.redismock._
import com.redis._
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play._
import play.api.Configuration
import scala.concurrent.ExecutionContext.Implicits.global
import scala.util.Random
import models._


trait MockRedisServer {

   val randomPort = 23000 + Random.nextInt(3000)
   lazy val redisServer = RedisServer.newRedisServer(randomPort)

   def startRedis(){
      redisServer.start()
   }

   def stopRedis(){
      redisServer.stop()
   }

   def redisConfiguration =
      Map(
         "database.redis.host" -> redisServer.getHost(),
         "database.redis.port" -> redisServer.getBindPort()
      )
}



class RateRepositoryComponentSpec extends PlaySpec with ScalaFutures with IntegrationPatience
with BeforeAndAfterEach with BeforeAndAfterAll with MockRedisServer {

   def given = afterWord("given")

   override def beforeAll() {
      startRedis()
      super.beforeAll()
   }

   override def afterAll() {
      stopRedis()
      super.afterAll()
   }

   // override def beforeEach() {
   //    super.beforeEach()
   // }
   //
   // override def afterEach() {
   //    super.afterEach()
   // }

   trait Setup {

      val dbConfig = new DatabaseConfiguration{
         // val appConfig = new ApplicationConfiguration(){
            val rootConfig = Configuration.from(redisConfiguration)
         // }
      }

      val rateRepository = new DefaultRateRepository(databaseConfig = dbConfig)

   }

   "RateRepository" should {
      "saveCryptoPerFiatRate" when given {
         "anything" in new Setup {

            val rate = CryptoPerFiatRate(
               CryptoCurrency.BTC, FiatCurrency.USD, BigDecimal("14500"))

            whenReady(rateRepository.saveCryptoPerFiatRate(rate)){ _ =>



               assert(1==2)
            }
         }
      }
   }

}
