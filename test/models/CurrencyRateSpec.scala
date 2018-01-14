package models

import java.time.LocalDateTime
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.play._
import Currency._

class CurrencyRateSpec extends PlaySpec with MockitoSugar with ScalaFutures
with TableDrivenPropertyChecks {

   def given = afterWord("given")

   val btc = BTC
   val eth = ETH
   val usd = USD

   trait Setup {
      val date = LocalDateTime.now
      val source = None
      val defaultRate = "12.34"
   }

   val rateScales =
      Table (
         ("dividen", "divisor", "rate", "inverse", "description"),
         ("BTC", "USD", "1.32",     "0.758",    "two decimals"),
         ("BTC", "USD", "1.32123",  "0.756870", "lots of decimals"),
         ("BTC", "USD", "1321232",  "0.0000008", "big number"),
         ("BTC", "USD", "13212.23", "0.0000757", "big with decimal")
      )

   val canInvert =
      Table (
         ("dividend", "divisor", "canInvert"),
         (USD,  GBP, true),
         (BTC, USD, true),
         (USDT, BTC, true),
         (ETH, BTC, false),
         (XRP, BTC, false)
      )

   "CurrencyRate" should {
      "inverse rate with correct scale" when given {
         forAll(rateScales) { (dividen: String, divisor: String, rate: String, inverse: String, description: String) =>
            s"${dividen} / ${divisor} and ${divisor} / ${dividen} with rate that $description" in new Setup {

               val currencyRate = CurrencyRate(
                  Currency.withName(dividen),
                  Currency.withName(divisor),
                  date,
                  BigDecimal(rate),
                  source)

               val inverseRate = currencyRate.inverse

               currencyRate.rate mustBe BigDecimal(rate)
               inverseRate.map(_.rate) mustBe Some(BigDecimal(inverse))

            }
         }
      }

      "inverse some but not all currencies" when given {
         forAll(canInvert) { (dividen, divisor, canInvert) =>
            s"$dividen / $divisor and $divisor / $dividen which can $canInvert" in new Setup {

                  val currencyRate = CurrencyRate( dividen, divisor, date, BigDecimal(defaultRate), source)

                  if(canInvert) currencyRate.inverse mustBe defined
                  else currencyRate.inverse mustBe None

            }
         }
      }
   }

}
