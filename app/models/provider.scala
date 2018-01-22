package models

import com.google.inject.ImplementedBy
import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}
import scala.concurrent.{ExecutionContext, Future}
import connectors._
import Currency._


trait ApiProvider {

   def findRate(pair: RatePair)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]]

}

@ImplementedBy(classOf[DefaultCryptoWatchApi])
trait CryptoWatchApi extends ApiProvider with WithLogger {

   def cryptoWatchConnector: CryptoWatchConnector

   def configuration: ApiProviderConfiguration
   //
   // def listsExchange(exchange: RateSource) = true
   //
   // def hasPair(pair: RatePair) = true

   def findRate(pair: RatePair)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]] = {
      configuration.findRateUrl( pair )
         .fold[Future[Option[CurrencyRate]]]{
            Future.successful(None)
         }{ pairSource =>
            // logger.debug("Url is " + pairSource.url)
            cryptoWatchConnector.findRate(pairSource.url)
               .map{ rate =>
                  Some( CurrencyRate( pair, LocalDateTime.now, rate, Some(pairSource.source) ) )
               }
         }
   }

}

@Singleton
class DefaultCryptoWatchApi @Inject() (
   val cryptoWatchConnector: CryptoWatchConnector,
   val configuration: ApiProviderConfiguration) extends CryptoWatchApi
