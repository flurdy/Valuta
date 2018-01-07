package models

import CryptoCurrency._
import FiatCurrency._


case class CurrencyPairRates(
   toUsd: Option[BigDecimal],
   toGbp: Option[BigDecimal],
   toEur: Option[BigDecimal],
   toBtc: Option[BigDecimal]
){
   def this(lookup: Map[Currency, Option[BigDecimal]]) =
      this(
         toUsd = lookup.get(USD).flatten,
         toGbp = lookup.get(GBP).flatten,
         toEur = lookup.get(EUR).flatten,
         toBtc = lookup.get(BTC).flatten
      )
}

case class CurrencyRates[A <: Currency](
   currency: A, rates: CurrencyPairRates)

case class Rates(
   cryptoRates: List[CurrencyRates[CryptoCurrency]],
   fiatRates: List[CurrencyRates[FiatCurrency]])

case class CryptoPerCryptoRate(dividen: CryptoCurrency, divisor: CryptoCurrency, rate: BigDecimal)

case class CryptoPerFiatRate(dividen: CryptoCurrency, divisor: FiatCurrency, rate: BigDecimal)

case class FiatPerFiatRate(dividen: FiatCurrency, divisor: FiatCurrency, rate: BigDecimal)
