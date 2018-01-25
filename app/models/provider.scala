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


@ImplementedBy(classOf[DefaultApiProviderLookup])
trait ApiProviderLookup {

   def configuration: ApiProviderConfiguration

   def cryptoWatchApi: CryptoWatchApi

   def fixerIoApi: FixerIoApi

   def findProvider(pair: RatePair)(implicit ec: ExecutionContext): Option[ApiProvider] =
      configuration.findSources(pair).headOption.map {
         case RateSource.Gdax => cryptoWatchApi
         case RateSource.FixerIo  => fixerIoApi
         case _ => throw new IllegalStateException(s"No provider for pair: $pair")
      }

}

@Singleton
class DefaultApiProviderLookup @Inject() (
   val configuration: ApiProviderConfiguration,
   val cryptoWatchApi: CryptoWatchApi,
   val fixerIoApi: FixerIoApi
) extends ApiProviderLookup


trait ApiProvider extends WithLogger {

   def connector: ApiConnector

   def configuration: ApiProviderConfiguration

   def findRate(pair: RatePair)(implicit ec: ExecutionContext): Future[Option[CurrencyRate]] =
      configuration.findRateUrl( pair )
         .fold[Future[Option[CurrencyRate]]]{
            Future.successful(None)
         }{ pairSource =>
            connector.findRate(pairSource.url, pair)
               .map{ rate =>
                  Some( CurrencyRate( pair, LocalDateTime.now, rate, Some(pairSource.source) ) )
               }.recover {
                  case e =>
                     logger.error(s"Unabled to find rate for ${pairSource.url}",e)
                     None
               }
         }

}


@ImplementedBy(classOf[DefaultCryptoWatchApi])
trait CryptoWatchApi extends ApiProvider

@Singleton
class DefaultCryptoWatchApi @Inject() (
   val connector: CryptoWatchConnector,
   val configuration: ApiProviderConfiguration) extends CryptoWatchApi


@ImplementedBy(classOf[DefaultFixerIoApi])
trait FixerIoApi extends ApiProvider

@Singleton
class DefaultFixerIoApi @Inject() (
   val connector: FixerIoConnector,
   val configuration: ApiProviderConfiguration) extends FixerIoApi
