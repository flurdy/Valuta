package services
//
import com.google.inject.ImplementedBy
import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import models._
import Currency._
import repositories._


@ImplementedBy(classOf[DefaultRateService])
trait RateService {

   def rateRepository: RateRepository

   implicit val rateReadRepository: RateReadRepository = rateRepository
   implicit val rateWriteRepository: RateWriteRepository = rateRepository

   def findRates()(implicit ec: ExecutionContext): Future[Rates] = {

      def findCurrencyRates(dividen: Currency, divisors: List[Currency]): Future[List[CurrencyRate]] =
         Future.sequence {
            divisors.toList.map { divisor =>
               rateRepository.findCurrencyRate(dividen, divisor)
            }
         }.map( _.flatten )

      def extractCurrencyRates(dividen: Currency, currencyRates: List[CurrencyRate]): CurrencyRates =
         CurrencyRates(dividen, currencyRates.map( c => (c.date, c)))

      val rates: Future[Map[Currency,CurrencyRates]] =
         Future.sequence {
            Currency.values.map{ dividen =>
               findCurrencyRates(dividen, Currency.divisorCurrencies.toList)
                        .map ( c => extractCurrencyRates(dividen, c) )
                        .map ( c => (dividen, c) )
            }
         }.map{
            _.toMap
             .filter( !_._2.rates.isEmpty )
         }
      rates.map(Rates(_))
   }

   def findDivisors(): Future[List[Currency]] = {
      Future.successful( Currency.divisorCurrencies.toList )
   }

   def findCurrencies(): Currencies =
      Currencies(Currency.CryptoCurrencies, Currency.FiatCurrencies, Currency.divisorCurrencies.toList)

}

@Singleton
class DefaultRateService @Inject() ()(implicit val rateRepository: RateRepository) extends RateService
