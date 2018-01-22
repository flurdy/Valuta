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

   val formatted =
      Table (
         ("rate","formattedRate"),
         ("0.1","0.1"),
         ("0.01","0.01"),
         ("0.001","0.001"),
         ("0.0001","0.0001"),
         ("0.123","0.123"),
         ("0.0123","0.0123"),
         ("0.01230","0.01230"),
         ("0.00123","0.00123"),
         ("0.000123","0.000123"),
         ("0.0000123","0.0000123"),
         ("0.00001230","0.00001230"),
         ("0.1234567","0.1235"),
         ("0.01234567","0.01235"),
         ("0.001234567","0.001235"),
         ("0.0001234567","0.0001235"),
         ("0.00001234567","0.00001235"),
         ("1.23","1.23"),
         ("12.3","12.3"),
         ("1.234567","1.235"),
         ("12.34567","12.35"),
         ("123.4567","123.5"),
         ("1234.567","1,235"),
         ("12345.67","12,346"),
         ("123456.7","123,457"),
         ("123456789012.1234","123,456,789,012"),
         ("1234567", "1,234,567"),
         ("12345000", "12,345,000"),
         ("123450000", "123,450,000")
      )
   "CurrencyRate" should {
      "inverse rate with correct scale" when given {
         forAll(rateScales) { (dividen: String, divisor: String, rate: String, inverse: String, description: String) =>
            s"${dividen} / ${divisor} and ${divisor} / ${dividen} with rate that $description" in new Setup {

               val currencyRate = CurrencyRate(
                  RatePair(
                     Currency.withName(dividen),
                     Currency.withName(divisor)),
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

                  val currencyRate = CurrencyRate( RatePair(dividen, divisor), date, BigDecimal(defaultRate), source)

                  if(canInvert) currencyRate.inverse mustBe defined
                  else currencyRate.inverse mustBe None

            }
         }
      }

      "formattedRate" should {
         forAll(formatted) { (rate, formattedRate) =>
            s"$rate formatted to $formattedRate" in {
               val currencyRate = CurrencyRate(
                     RatePair(ETH, USD), LocalDateTime.now,
                     BigDecimal(rate), None)
               currencyRate.formattedRate mustBe formattedRate
            }
         }
      }
   }
}
