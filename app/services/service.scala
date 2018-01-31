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
              .sortWith( _.entryName > _.entryName )
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
         // _ = ratesConverted.map( r => logger.debug(s"converted pair ${r.pair}") )
         filteredConverts =  ratesConverted.filter( rate => !pairsSaved.contains(rate.pair) )
         // _ = filteredConverts.map( r => logger.debug(s"filtered pair ${r.pair}") )
         savedConverted   <- saveRates(filteredConverts)
         _ = logger.info(
              s"Saved ${savedFiatRates.size} fiat rates, "
            + s"${savedCryptoRates.size} crypto rates, "
            + s"${ratesConverted.size} unfiltered rates, "
            + s"${savedConverted.size} converted rates")
      } yield ()
   }
}

@Singleton
class DefaultRateService @Inject() ()(implicit val rateRepository: RateRepository) extends RateService
