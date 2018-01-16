package models

import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.time.LocalDateTime
import scala.concurrent.{ExecutionContext, Future}
import repositories._

sealed trait CurrencyType
case object FiatCurrency  extends CurrencyType
case object CryptoCurrency extends CurrencyType


sealed abstract class Currency(val description: String, val currencyType: CurrencyType, val isDivisor: Boolean) extends EnumEntry {
   val isCryptoCurrency = currencyType == CryptoCurrency
   val isFiatCurrency = currencyType == FiatCurrency

   def findDivisors()(implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[List[Currency]] =
      rateRepository.findDivisorsForCurrency(this)

   def findRatesByDates()(implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[DateRates] =
      rateRepository.findRatesByDates(this)

}

case object Currency extends PlayEnum[Currency] {

   val values = findValues
   val CryptoCurrencies  = values.filter(_.isCryptoCurrency)
   val FiatCurrencies    = values.filter(_.isFiatCurrency)
   val divisorCurrencies = (FiatCurrencies union CryptoCurrencies).filter(_.isDivisor)

   case object BTC  extends Currency(description = "Bitcoin",
                                     currencyType = CryptoCurrency, isDivisor = true)
   case object ETH  extends Currency(description = "Ethereum",
                                     currencyType = CryptoCurrency, isDivisor = false)
   case object XRP  extends Currency(description = "Ripple",
                                     currencyType = CryptoCurrency, isDivisor = false)
   case object USDT extends Currency(description = "Tether",
                                     currencyType = CryptoCurrency, isDivisor = true)
   case object USD  extends Currency(description = "US Dollars",
                                     currencyType = FiatCurrency,   isDivisor = true)
   case object GBP  extends Currency(description = "British Pound",
                                     currencyType = FiatCurrency,   isDivisor = true)
   case object EUR  extends Currency(description = "Euros",
                                     currencyType = FiatCurrency,   isDivisor = true)

}

case class Currencies(cryptoCurrencies: Seq[Currency], fiatCurrencies: Seq[Currency], divisors: Seq[Currency])
