package models

import com.google.inject.ImplementedBy
import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import connectors._


sealed abstract class RateSource extends EnumEntry

case object RateSource extends PlayEnum[RateSource]{
   val values = findValues
   case object Bitfinex      extends RateSource
   case object Poloniex      extends RateSource
   case object Gemini        extends RateSource
   case object Gdax          extends RateSource
   case object Bittrex       extends RateSource
   case object Bitstamp      extends RateSource
   case object Kraken        extends RateSource
   case object Binance       extends RateSource
   case object CoinmarketCap extends RateSource
   case object CryptoWatch   extends RateSource
   case object ECB           extends RateSource
   case object Calculated    extends RateSource
   case object Manual        extends RateSource
   case object None          extends RateSource
}

case class RatePairSource(source: RateSource, pair: RatePair, url: String)
