package models

import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.time.{LocalDateTime, ZoneOffset}
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode
import Currency._
import repositories._


sealed trait RateSource extends EnumEntry
case object RateSource extends PlayEnum[RateSource]{
   val values = findValues
   case object Bitfinex extends RateSource
   case object Poloniex extends RateSource
   case object Gemini   extends RateSource
   case object Gdax     extends RateSource
   case object Bittrex  extends RateSource
   case object Bitstamp extends RateSource
   case object Kraken   extends RateSource
   case object Binance  extends RateSource
   case object CoinmarketCap extends RateSource
   case object CryptoWatch   extends RateSource
   case object Calculated    extends RateSource
   case object None     extends RateSource
}

case class CurrencyRate(dividen: Currency, divisor: Currency, date: LocalDateTime, rate: BigDecimal, source: Option[RateSource] = None){

   val epochSecond = date.toEpochSecond(ZoneOffset.UTC)

   def enterNewRate()(implicit ec: ExecutionContext, rateWriteRepository: RateWriteRepository): Future[Unit] =
      rateWriteRepository.saveCurrencyRate(this)

   def inverse =
      if(dividen.isDivisor)
         Some(this.copy( dividen = divisor, divisor = dividen,
                           rate = inverseRate))
      else None

   def inverseRate = {
      val scale =
         if(rate.precision > rate.scale)
            rate.precision
         else rate.scale
      (BigDecimal("1") / rate).setScale(scale, RoundingMode.HALF_UP)
   }
}

case class CurrencyRates(dividen: Currency, rates: List[(LocalDateTime,CurrencyRate)])

case class DivisorRates(rates: Map[Currency,CurrencyRate])

case class DateRates(rates: Map[LocalDateTime,DivisorRates])

case class Rates(rates: Map[Currency,CurrencyRates])
