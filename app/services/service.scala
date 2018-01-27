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
trait RateService extends WithLogger {

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

   def findDivisors(rates: Rates): Future[List[Currency]] =
      Future.successful{
         rates.rates
              .values
              .toList
              .map ( _.rates )
              .map { cr =>
                 cr.map(_._2)
              }.flatten
              .map ( _.pair.divisor )
              .toSet
              .toList
     }

   def findCurrencies(): Currencies =
      Currencies(Currency.CryptoCurrencies, Currency.FiatCurrencies, Currency.divisorCurrencies.toList)

   def fetchAllRates()(implicit ec: ExecutionContext,
         providerConf: ApiProviderConfiguration,
         apiProviderLookup: ApiProviderLookup,
         rateWriteRepository: RateWriteRepository): Future[Unit] = {

      def findPairsWithSource(currencies: List[Currency]): List[RatePair] =
         currencies.map { currency =>
                     currency.findDivisorsWithSources()
                             .map( RatePair(currency,_) )
                  }.flatten

      def findPairsPossible(currencies: List[Currency]): List[RatePair] =
         currencies.map { currency =>
                     currency.findDivisorsPossible()
                             .map( RatePair(currency,_) )
                  }.flatten

      def findRates(pairs: List[RatePair]): Future[List[CurrencyRate]] =
         Future.sequence {
            pairs.map ( _.fetchRate() )
         }.map ( _.flatten )

      def saveRates(rates: List[CurrencyRate]): Future[List[CurrencyRate]] =
         Future.sequence{
            rates.map ( _.save() )
         }

      def convertRates(rates: List[CurrencyRate]): Future[List[CurrencyRate]] =
         Future.sequence{
            rates.map ( _.convertInverseAndConvert() )
         }.map( _.flatten )

      def filterConverts( pairs: Set[RatePair], converted: List[CurrencyRate]): List[CurrencyRate] =
         converted.filter( rate => pairs.contains(rate.pair) )

      val fiatSourcePairs   = findPairsWithSource(Currency.FiatCurrencies.toList)
      val cryptoSourcePairs = findPairsWithSource(Currency.CryptoCurrencies.toList)
      for{
         fiatRates        <- findRates(fiatSourcePairs)
         savedFiatRates   <- saveRates(fiatRates)
         cryptoRates      <- findRates(cryptoSourcePairs)
         savedCryptoRates <- saveRates(cryptoRates)
         savedSourceRates =  savedFiatRates union savedCryptoRates
         pairsSaved       =  savedSourceRates.map ( _.pair ).toSet
         ratesConverted   <- convertRates( savedSourceRates )
         filteredConverts =  filterConverts(pairsSaved, ratesConverted)
         savedConverted   <- saveRates(filteredConverts)
      } yield ()
   }
}

@Singleton
class DefaultRateService @Inject() ()(implicit val rateRepository: RateRepository) extends RateService
