package connectors

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.libs.ws._
import scala.concurrent.{ExecutionContext, Future}
import scala.concurrent.duration._
import models._
import play.api.libs.json._

case class CryptoWatchPrice(price: BigDecimal)

case class CryptoWatchPriceResponse(result: CryptoWatchPrice)


trait ApiConnector {

   def ws: WSClient

   def findRate(url: String, pair: RatePair)(implicit ec: ExecutionContext): Future[BigDecimal]

}


@ImplementedBy(classOf[DefaultCryptoWatchConnector])
trait CryptoWatchConnector extends ApiConnector {

   implicit val CryptoWatchPriceReads = Json.reads[CryptoWatchPrice]
   implicit val CryptoWatchPriceResponseReads = Json.reads[CryptoWatchPriceResponse]

   override def findRate(url: String, pair: RatePair)(implicit ec: ExecutionContext): Future[BigDecimal] =
      ws.url(url)
        .withFollowRedirects(true)
        .withRequestTimeout(2000.millis)
        .get()
        .map{ r =>
           (r.json \ "result" \ "price").as[BigDecimal]
        }

}

@Singleton
class DefaultCryptoWatchConnector @Inject() (val ws: WSClient) extends CryptoWatchConnector


@ImplementedBy(classOf[DefaultFixerIoConnector])
trait FixerIoConnector extends ApiConnector with WithLogger {

   override def findRate(url: String, pair: RatePair)(implicit ec: ExecutionContext): Future[BigDecimal] =
      ws.url(url)
        .withFollowRedirects(true)
        .withRequestTimeout(2000.millis)
        .get()
        .map{ r =>
           (r.json \ "rates" \ s"${pair.divisor.entryName.toUpperCase}" ).as[BigDecimal]
        }

}

@Singleton
class DefaultFixerIoConnector @Inject() (val ws: WSClient) extends FixerIoConnector
