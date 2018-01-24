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


@ImplementedBy(classOf[DefaultCryptoWatchApi])
trait ApiProvider {

   def findRate(pair: RatePair)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]]

}

trait CryptoWatchApi extends ApiProvider with WithLogger {

   def cryptoWatchConnector: CryptoWatchConnector

   def configuration: ApiProviderConfiguration

   def findRate(pair: RatePair)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]] = {
      configuration.findRateUrl( pair )
         .fold[Future[Option[CurrencyRate]]]{
            Future.successful(None)
         }{ pairSource =>
            cryptoWatchConnector.findRate(pairSource.url)
               .map{ rate =>
                  Some( CurrencyRate( pair, LocalDateTime.now, rate, Some(pairSource.source) ) )
               }.recover {
                  case e =>
                     logger.error(s"Unabled to find rate for ${pairSource.url}",e)
                     None
               }
         }
   }

}

@Singleton
class DefaultCryptoWatchApi @Inject() (
   val cryptoWatchConnector: CryptoWatchConnector,
   val configuration: ApiProviderConfiguration) extends CryptoWatchApi
