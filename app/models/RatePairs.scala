package models

import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.text.DecimalFormat
import java.time.{LocalDateTime, ZoneOffset}
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode
import scala.math._
import Currency._
import repositories._


case class RatePair(dividen: Currency, divisor: Currency){

   def inverse = RatePair(divisor, dividen)

   def fetchRate()(implicit ec: ExecutionContext, apiProvider: ApiProvider): Future[Option[CurrencyRate]] =
      apiProvider.findRate(this)
}


case class CurrencyRate(pair: RatePair, date: LocalDateTime,
                        rate: BigDecimal, source: Option[RateSource] = None) extends WithLogger {

   val epochSecond = date.toEpochSecond(ZoneOffset.UTC)

   def enterNewRate()(implicit ec: ExecutionContext, rateWriteRepository: RateWriteRepository): Future[CurrencyRate] =
      rateWriteRepository.saveCurrencyRate(this)

   def inverse =
      if(pair.dividen.isDivisor)
         Some(this.copy( pair = pair.inverse, rate = inverseRate))
      else None

   def inverseRate = {
      val scale =
         if(rate.precision > rate.scale)
            rate.precision
         else rate.scale
      (BigDecimal("1") / rate).setScale(scale, RoundingMode.HALF_UP)
   }

   val SignificantNumbers = 4

   def formattedRate: String = {
      val newScale =
         if( rate.scale <= rate.precision - SignificantNumbers ) 0
         else if(rate.scale < SignificantNumbers ) rate.scale
         else if(rate.precision < SignificantNumbers ) rate.scale
         else SignificantNumbers + rate.scale - rate.precision
      val formatter = java.text.NumberFormat.getInstance
      formatter.setMinimumFractionDigits(newScale);
      formatter.format(rate.setScale( newScale, RoundingMode.HALF_UP))
   }
}

case class CurrencyRates(dividen: Currency, rates: List[(LocalDateTime,CurrencyRate)])

case class DivisorRates(rates: Map[Currency,CurrencyRate])

case class DateRates(rates: Map[LocalDateTime,DivisorRates]){
   val sortedKeys = rates.keys.toList.sortWith(_.toEpochSecond(ZoneOffset.UTC) > _.toEpochSecond(ZoneOffset.UTC))
}

case class Rates(rates: Map[Currency,CurrencyRates])
