package models

import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.text.DecimalFormat
import java.time.{LocalDate, LocalDateTime, ZoneOffset}
import java.time.temporal.ChronoUnit
import scala.concurrent.{ExecutionContext, Future}
import scala.math.BigDecimal.RoundingMode
import scala.math._
import Currency._
import repositories._


case class RatePair(dividen: Currency, divisor: Currency) extends WithLogger {

   def inverse = RatePair(divisor, dividen)

   val isFiatPair = dividen.isFiatCurrency && divisor.isFiatCurrency

   private def lookupRate()(implicit ec: ExecutionContext, apiProviderLookup: ApiProviderLookup): Future[Option[CurrencyRate]] =
      apiProviderLookup.findProvider(this).fold[Future[Option[CurrencyRate]]]{
         Future.successful(None)
      }( _.findRate(this) )

   def fetchRate()(implicit ec: ExecutionContext, apiProviderLookup: ApiProviderLookup,
            rateRepository: RateReadRepository): Future[Option[CurrencyRate]] =
      findRate().flatMap {
         case Some(rateFound) if isFiatPair && rateFound.isFromToday() =>
            Future.successful(Some(rateFound))
         case Some(rateFound) if rateFound.isFromThisQuarterHour() =>
            Future.successful(Some(rateFound))
         case _ =>
            lookupRate()
      }

   def findRate()(implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[Option[CurrencyRate]] =
      rateRepository.findCurrencyRate(this)

   def isCachedAlready()(implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[Boolean] =
      findRate().map { rate =>
         rate.fold(false)( _.isRecentEnough() )
      }
}


case class CurrencyRate(pair: RatePair, date: LocalDateTime,
                        rate: BigDecimal, source: Option[RateSource] = None, sourcedFrom: Option[SourcedFrom] = None ) extends WithLogger {

   val epochSecond = date.toEpochSecond(ZoneOffset.UTC)

   def isFromToday(now: LocalDate = LocalDate.now) = date.toLocalDate.isAfter(now.minusDays(1))

   def isFromThisHour(now: LocalDateTime = LocalDateTime.now) = date.isAfter(now.truncatedTo(ChronoUnit.HOURS))

   def isFromThisQuarterHour(now: LocalDateTime = LocalDateTime.now) =
      date.isAfter(
         now.truncatedTo(ChronoUnit.HOURS)
            .plusMinutes( 15 * (now.getMinute / 15 ))
      )

   def isFromThisMinute = date.isAfter(LocalDateTime.now.truncatedTo(ChronoUnit.MINUTES))

   def save()(implicit ec: ExecutionContext, rateWriteRepository: RateWriteRepository): Future[CurrencyRate] = {
      logger.debug(s"saving ${this.sourcedFrom}")
      rateWriteRepository.saveCurrencyRate(this)
   }

   def inverse =
      if(pair.dividen.isDivisor)
         Some(this.copy( pair = pair.inverse, rate = inverseRate))
      else None

   protected def inverseRate = {
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

   def convertInverseAndConvert()(implicit ec: ExecutionContext,
            rateRepository: RateReadRepository,
            providerConfig: ApiProviderConfiguration): Future[List[CurrencyRate]] =
      for{
         converts <- convertToOtherDivisors()
         inverses <- inverseAndConvert()
      } yield converts ::: inverses

   def inverseAndConvert()(implicit ec: ExecutionContext,
            rateRepository: RateReadRepository,
            providerConfig: ApiProviderConfiguration): Future[List[CurrencyRate]] =
      inverse.fold[Future[List[CurrencyRate]]]{
         Future.successful(List())
      }{ i =>
         i.convertToOtherDivisors()
          .map( i :: _ )
      }

   def convertToOtherDivisors()(implicit ec: ExecutionContext,
            rateRepository: RateReadRepository,
            providerConfig: ApiProviderConfiguration): Future[List[CurrencyRate]] = {

      val pairsToConvert =
         for {
            divisorDivisors <- pair.divisor.findDivisorsUsed()
            divisorDividens <- pair.divisor.findDividensUsed()
         } yield {
           pair.dividen.findDivisorsPossible()
                       .filter( _ != pair.divisor )
                       .map { divisor =>
                          if( divisorDivisors.exists( _ == divisor ) )
                             Some( RatePair( pair.divisor , divisor ) )
                          else if( divisorDividens.exists( _ == divisor ) )
                             Some( RatePair( divisor, pair.divisor ) )
                          else {
                             None
                          }
                       }.flatten
         }

      val ratesToConvert = pairsToConvert.map { pairs =>
            pairs.map ( _.findRate() )
         }.map( Future.sequence(_) )
          .flatten
          .map( _.flatten )

      ratesToConvert map { rates =>
         rates.map {
            case CurrencyRate( RatePair(convertDividen, convertDivisor), _, convertRate, _, _) =>
               if (convertDividen == pair.divisor) {
                  val newRate = convertRate * rate
                  this.copy( pair = RatePair(pair.dividen, convertDivisor),
                             rate = newRate,
                             source = Some(RateSource.Calculated),
                             sourcedFrom = Some(SourcedFrom.FromCalculated) )
               } else if (convertDivisor == pair.divisor) {
                  val newRate = rate / convertRate
                  this.copy( pair = RatePair(pair.dividen, convertDividen),
                             rate = newRate,
                             source = Some(RateSource.Calculated),
                             sourcedFrom = Some(SourcedFrom.FromCalculated) )
               } else {
                  throw new IllegalStateException(s"Oh dear, unexpted rate: $convertDividen / $convertDivisor")
               }
         }
      }
   }

   def isCachedAlready()(implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[Boolean] =
      pair.isCachedAlready()

   def isRecentEnough(now: LocalDateTime = LocalDateTime.now) =
      (pair.isFiatPair && isFromToday(now.toLocalDate)) || isFromThisQuarterHour(now)
}

case class CurrencyRates(dividen: Currency, rates: List[(LocalDateTime,CurrencyRate)])

case class DivisorRates(rates: Map[Currency,CurrencyRate])

case class DateRates(rates: Map[LocalDateTime,DivisorRates]){
   val sortedKeys = rates.keys.toList
         .sortWith(_.toEpochSecond(ZoneOffset.UTC) > _.toEpochSecond(ZoneOffset.UTC))
}

case class Rates(rates: Map[Currency,CurrencyRates])
