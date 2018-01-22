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

@ImplementedBy(classOf[DefaultCryptoWatchConnector])
trait CryptoWatchConnector {


   implicit val CryptoWatchPriceReads = Json.reads[CryptoWatchPrice]
   implicit val CryptoWatchPriceResponseReads = Json.reads[CryptoWatchPriceResponse]

   def ws: WSClient

   def findRate(url: String)(implicit ec: ExecutionContext): Future[BigDecimal] = {
      ws.url(url)
        .withFollowRedirects(true)
        .withRequestTimeout(2000.millis)
        .get()
        .map{ r =>
           (r.json \ "result" \ "price").as[BigDecimal]
        }
   }

}

@Singleton
class DefaultCryptoWatchConnector @Inject() (val ws: WSClient) extends CryptoWatchConnector
