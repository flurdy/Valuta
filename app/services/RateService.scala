package services

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import models._
import repositories._
import CryptoCurrency._
import FiatCurrency._


@ImplementedBy(classOf[DefaultRateService])
trait RateService {

   def rateRepository: RateRepository
   implicit val rateReadRepository: RateReadRepository = rateRepository
   implicit val rateWriteRepository: RateWriteRepository = rateRepository

   val FiatCurrencies: List[FiatCurrency] = List(USD, EUR, GBP)
   val PairCurrencies = List(USD, EUR, GBP, BTC)

   private def findRates[A <: Currency](currencies: List[A])
         (implicit ec: ExecutionContext): Future[List[CurrencyRates[A]]] =
      Future.sequence {
         currencies.map { currency =>
            currency.findPairRates[A](PairCurrencies).map { pairRates =>
               CurrencyRates( currency, pairRates )
            }
         }
      }

   private def findCryptoRates()(implicit ec: ExecutionContext): Future[List[CurrencyRates[CryptoCurrency]]] =
      rateReadRepository.findCryptoCurrencies().map { currencies =>
         findRates[CryptoCurrency](currencies)
      }.flatten

   private def findFiatRates()(implicit ec: ExecutionContext):
         Future[List[CurrencyRates[FiatCurrency]]] =
      findRates[FiatCurrency](FiatCurrencies)

   def findRates()(implicit ec: ExecutionContext): Future[Rates] =
      for {
         cryptoRates <- findCryptoRates()
         fiatRates   <- findFiatRates()
      } yield Rates(cryptoRates, fiatRates)

   def findCurrencies(): Currencies =
      Currencies(CryptoCurrency.values, FiatCurrency.values)

   def enterNewRate(rate: CryptoPerCryptoRate): Future[Unit] =
      rateWriteRepository.saveCryptoPerCryptoRate(rate)

   def enterNewRate(rate: CryptoPerFiatRate)(implicit ec: ExecutionContext): Future[Unit] =
      rateWriteRepository.saveCryptoPerFiatRate(rate)

   def enterNewRate(rate: FiatPerFiatRate): Future[Unit] =
      rateWriteRepository.saveFiatPerFiatRate(rate)
}

@Singleton
class DefaultRateService @Inject() ()(implicit val rateRepository: RateRepository) extends RateService
