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

   def findRates()(implicit ec: ExecutionContext, providerConf: ApiProviderConfiguration): Future[Rates] = {

      def extractCurrencyRates(dividen: Currency, currencyRates: List[CurrencyRate]): CurrencyRates =
         CurrencyRates(dividen, currencyRates.map( c => (c.date, c)))

      val rates: Future[Map[Currency,CurrencyRates]] =
         Future.sequence {
            Currency.values.map{ dividen =>
               dividen.findCurrencyRates()
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
