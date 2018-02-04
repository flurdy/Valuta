package models

import java.time.{LocalDate, LocalDateTime}
import java.time.temporal.ChronoUnit
import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.play._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.ExecutionContext.Implicits.global
import Currency._
import repositories._

class CurrencyRateSpec extends PlaySpec with MockitoSugar with ScalaFutures
with TableDrivenPropertyChecks {

   def given = afterWord("given")

   val btc = BTC
   val eth = ETH
   val usd = USD

   trait InverseSetup {
      val date = LocalDateTime.now
      val source = None
      val defaultRate = "12.34"
   }

   trait CachedSetup {
      val cryptoRate = CurrencyRate(RatePair(Currency.BTC, Currency.USD), LocalDateTime.now, BigDecimal("1.2"), None, None)
      val fiatRate = CurrencyRate(RatePair(Currency.GBP, Currency.USD), LocalDateTime.now, BigDecimal("1.2"), None, None)
      val now      = LocalDateTime.now
      val today    = now.toLocalDate
      val backThen = LocalDateTime.of(2014, 4, 15, 21, 47)
      val thatDay  = backThen.toLocalDate
      // implicit val rateRepository = mock[RateReadRepository]
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
         // (ADA, BTC, true),
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

   val convertRates =
      Table(
         ("sourceDividen", "sourceDivisor",
                   "targetDividen", "targetDivisor",
                              "interDividen", "interDivisor",
                                        "sourceRate", "targetRate", "targetFormat", "interRate"),
         (BTC, USD, BTC, GBP, GBP, USD, "13000", "10000", "10,000", "1.30" ),
         (ETH, BTC, ETH, USD, BTC, USD, "0.1", "1000", "1,000", "10000" ),
         (XRP, USD, XRP, BTC, BTC, USD, "0.005", "0.0000005", "0.0000005", "10000" )
      )

   "inverse" should {
      "inverse rate with correct scale" when given {
         forAll(rateScales) { (dividen: String, divisor: String, rate: String, inverse: String, description: String) =>
            s"${dividen} / ${divisor} and ${divisor} / ${dividen} with rate that $description" in new InverseSetup {

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
            s"$dividen / $divisor and $divisor / $dividen which can $canInvert" in new InverseSetup {

                  val currencyRate = CurrencyRate( RatePair(dividen, divisor), date, BigDecimal(defaultRate), source)

                  if(canInvert) currencyRate.inverse mustBe defined
                  else currencyRate.inverse mustBe None

            }
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

   "convertToOtherDivisors" should {
      forAll(convertRates) { (sourceDividen, sourceDivisor, targetDividen, targetDivisor, interDividen, interDivisor, sourceRate, targetRate, targetFormat, interRate) =>
         s"convert $sourceDividen/$sourceDivisor ($sourceRate) to $targetDividen/$targetDivisor ($targetFormat) via $interDividen/$interDivisor ($interRate)" in {

            implicit val rateRepositoryMock = mock[RateReadRepository]
            implicit val providerConfMock = mock[ApiProviderConfiguration]
            val now = LocalDateTime.now
            val sourcePair = RatePair(sourceDividen, sourceDivisor)
            val interPair  = RatePair(interDividen,  interDivisor)
            val targetPair = RatePair(targetDividen, targetDivisor)
            val sourceDecimal  = BigDecimal(sourceRate)
            val interDecimal   = BigDecimal(interRate)
            val targetDecimal  = BigDecimal(targetRate)

            val sourceCurrencyRate = CurrencyRate(
                  sourcePair,
                  date = now,
                  sourceDecimal,
                  source = Some(RateSource.Binance)
               )

            val interCurrencyRate = CurrencyRate(
                  interPair,
                  date = now.minusDays(2),
                  interDecimal,
                  source = Some(RateSource.Manual)
               )

            val targetCurrencyRate = CurrencyRate(
                  targetPair,
                  date = now,
                  targetDecimal,
                  source = Some(RateSource.Calculated),
                  sourcedFrom = Some(SourcedFrom.FromCalculated)
               )

            if( sourceDivisor == interDivisor ) {
               when( rateRepositoryMock.findDivisorsForDividen(sourceDivisor) )
                     .thenReturn(Future.successful( List() ))
               when( rateRepositoryMock.findDividensForDivisor(sourceDivisor) )
                     .thenReturn(Future.successful( List(targetDivisor) ))
            } else if( sourceDivisor == interDividen ) {
               when( rateRepositoryMock.findDivisorsForDividen(sourceDivisor) )
                     .thenReturn(Future.successful( List(targetDivisor) ))
               when( rateRepositoryMock.findDividensForDivisor(sourceDivisor) )
                     .thenReturn(Future.successful( List() ))
            }

            when( providerConfMock.findDivisors(sourceDividen) )
                  .thenReturn( List(targetDivisor) )

            when( rateRepositoryMock.findCurrencyRate(any[RatePair])(any[ExecutionContext])) //interPair))
                  .thenReturn( Future.successful( Some(interCurrencyRate) ) )

            whenReady( sourceCurrencyRate.convertToOtherDivisors() ){ convertedRates =>

               convertedRates.head.formattedRate mustBe targetCurrencyRate.formattedRate
               convertedRates.head mustBe targetCurrencyRate

               verify(rateRepositoryMock).findCurrencyRate(interPair)
            }
         }
      }
   }

   "isRecentEnough" when {
      "fiatCurrency and today as last rate date" in new CachedSetup {
         val rate = fiatRate.copy(date = today.atStartOfDay)
         rate.isRecentEnough(now) mustBe true
      }

      // "cryptoCurrency and this hour as last rate date" in new CachedSetup {
      //    val rate = cryptoRate.copy(date = backThen.minusMinutes(35))
      //    rate.isRecentEnough(backThen) mustBe true
      // }

      "cryptoCurrency and same quarter hour as last rate date" in new CachedSetup {
         val rate = cryptoRate.copy(date = backThen.withMinute(10))
         rate.isRecentEnough(backThen.withMinute(12)) mustBe true
      }

      "fiatCurrency and last week as last rate date" in new CachedSetup {
         val rate = fiatRate.copy(date = now.minusWeeks(1))
         rate.isRecentEnough(now) mustBe false
      }

      "fiatCurrency and yesterday as last rate date" in new CachedSetup {
         val rate = fiatRate.copy(date = now.minusDays(1))
         rate.isRecentEnough(now) mustBe false
      }

      "cryptoCurrency and earlier same day as last rate date" in new CachedSetup {
         val rate = cryptoRate.copy(date = backThen.withHour(4))
         rate.isRecentEnough(backThen.withHour(14)) mustBe false
      }

      "cryptoCurrency and earlier same hour as last rate date" in new CachedSetup {
         val rate = cryptoRate.copy(date = backThen.withMinute(10))
         rate.isRecentEnough(backThen.withMinute(50)) mustBe false
      }

      "cryptoCurrency and less than quarter an hour but different quarter hour as last rate date" in new CachedSetup {
         val rate = cryptoRate.copy(date = backThen.withMinute(10))
         rate.isRecentEnough(backThen.withMinute(20)) mustBe false
      }
   }
}
