package connectors

import java.time.LocalDateTime
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.OptionValues._
import org.scalatest.mockito.MockitoSugar
import org.scalatest.time._
import org.scalatestplus.play._
import play.api.libs.ws.ahc._
import play.api.test._
import scala.concurrent.ExecutionContext.Implicits.global
import models._
import Currency._
import RateSource._

class FixerIoConnectorIntegrationSpec extends PlaySpec with MockitoSugar with ScalaFutures with IntegrationPatience {

   implicit override val patienceConfig =
     PatienceConfig(timeout = scaled(Span(5, Seconds)), interval = scaled(Span(250, Millis)))

   "findRate" should {
      "find a rate" in {
         // WsTestClient.withClient { wsClient =>
         //    val url = "https://api.fixer.io/latest?base=GBP"
         //    val connector = new DefaultFixerIoConnector(wsClient)
         //    val rate = connector.findRate(url, RatePair(GBP, USD))
         //    whenReady( rate ){ rateFound =>
         //       rateFound must be > BigDecimal("1")
         //       rateFound must be < BigDecimal("1000000")
         //    }
         // }
         pending
      }
   }
}
