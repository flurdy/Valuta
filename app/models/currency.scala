package models

import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import scala.concurrent.{ExecutionContext, Future}
import repositories._


sealed trait Currency extends WithLogger {

   def findCurrentRate(pairCurrency: Currency): Future[Option[BigDecimal]] = {
      logger.info(s"LOOKING UP current rate for $this & $pairCurrency")
      Future.successful( None )
   }

   def findPairRate[A <: Currency](pairCurrency: Currency)
         (implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[Option[BigDecimal]] =
      rateRepository.findCurrencyPairRate(this, pairCurrency).flatMap {
         case rate @ Some(_) => Future.successful(rate)
         case _ => this.findCurrentRate(pairCurrency)
      }

   def findPairRates[A <: Currency](pairCurrencies: List[Currency])
         (implicit ec: ExecutionContext, rateRepository: RateReadRepository): Future[CurrencyPairRates] =
      Future.sequence {
         pairCurrencies
            .filter(_ != this)
            .map { pairCurrency =>
               this.findPairRate[A](pairCurrency) .map ( (pairCurrency, _) )
            }
      }
      .map( _.filter( v => v._2.isDefined) )
      .map(_.toMap)
      .map{ pairRates =>
         println("Rates"  + pairRates)
         new CurrencyPairRates(pairRates)
      }

}


sealed abstract class CryptoCurrency(val value: String, name: String) extends StringEnumEntry with Currency

object CryptoCurrency extends StringPlayEnum[CryptoCurrency] {
   val values = findValues
   case object BTC        extends CryptoCurrency("BTC", "Bitcoin")
   case object BCH        extends CryptoCurrency("BCH", "Bitcoin Cash")
   case object ETH        extends CryptoCurrency("ETH", "Ethereum")
   case object LTC        extends CryptoCurrency("LTC", "LiteCoin")
   case object XRP        extends CryptoCurrency("XRP", "Ripple")
   case object ADA        extends CryptoCurrency("ADA", "Cardano")
   case object IOTA       extends CryptoCurrency("IOTA", "IOTA")
   // case object BTG        extends CryptoCurrency("Bitcoin Gold") with Words
   // // case object EthereumClassic extends CryptoCurrency("ETC") with Camelcase
   // case object DASH       extends CryptoCurrency("DASH")
   // case object XRM        extends CryptoCurrency("Monero")
   // // case object Qtum            extends CryptoCurrency("QTUM")
   // case object XEM        extends CryptoCurrency("NEM")
   // // case object Neo             extends CryptoCurrency("NEO")
   // case object EOS        extends CryptoCurrency("EOS")
   // case object Stellar         extends CryptoCurrency("XLM")
   // case object Verge           extends CryptoCurrency("XVG")
   // case object Einsteinium     extends CryptoCurrency("EMC2")
   // case object BTCD       extends CryptoCurrency("Bitcoin Dark") with Words
}


sealed abstract class FiatCurrency(val value: String, name: String) extends StringEnumEntry with Currency

object FiatCurrency extends StringPlayEnum[FiatCurrency] {
   val values = findValues
   case object USD  extends FiatCurrency("USD", "US Dollars")
   case object EUR  extends FiatCurrency("EUR", "Euros")
   case object GBP  extends FiatCurrency("GBP", "British Pounds")
   case object NOK  extends FiatCurrency("NOK", "Norwegian Kroners")
}


case class Currencies(cryptoCurrencies: Seq[CryptoCurrency], fiatCurrencies: Seq[FiatCurrency])
