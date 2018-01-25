package models

import java.time.LocalDateTime
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.mockito.MockitoSugar
import org.scalatestplus.play._
import play.api.libs.ws.ahc._
import play.api.test._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.{ExecutionContext, Future}
import Currency._
import connectors._

class FixerIoApiSpec extends PlaySpec with MockitoSugar with ScalaFutures {

   trait Setup {
      val connectorMock = mock[FixerIoConnector]
      val configurationMock = mock[ApiProviderConfiguration]
      val api = new FixerIoApi {
         val connector = connectorMock
         val configuration = configurationMock
      }
      val pair = RatePair(GBP, USD)
      val url = "http://someUrl"
      val rateSource = RatePairSource(RateSource.Ecb, pair, url)
      val rate = BigDecimal("1.23")
   }

   "findRate" should {
      "find a rate" in new Setup {

         when(configurationMock.findRateUrl(pair)).thenReturn(Some(rateSource))
         when(connectorMock.findRate(url,pair)).thenReturn(Future.successful(rate))

         whenReady( api.findRate( pair ) ){ result =>

            result mustBe defined
            result.value.rate mustBe rate
            result.value.pair mustBe pair
            result.value.source.value mustBe RateSource.Ecb
         }
      }

      "given no source not find rate" in new Setup {

         when(configurationMock.findRateUrl(pair)).thenReturn(None)

         whenReady( api.findRate( pair ) ){ result =>

            result mustBe None
            verify( connectorMock, never ).findRate(anyString,any[RatePair])(any[ExecutionContext])
         }
      }

      "given no response not find rate" in new Setup {
         when(configurationMock.findRateUrl(pair))
               .thenReturn(Some(rateSource))
         when(connectorMock.findRate(url,pair))
               .thenReturn(Future.failed(new RuntimeException("nope")))

         whenReady( api.findRate( pair ) ){ result =>

            result mustBe None
         }
      }
   }
}
