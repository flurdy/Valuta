
import akka.stream.Materializer
import javax.inject.{Inject, Singleton}
import play.api.http.DefaultHttpFilters
import play.api.mvc._
import play.filters.gzip.GzipFilter
import play.filters.headers.SecurityHeadersFilter
import scala.concurrent.{ExecutionContext, Future}
import models.WithLogger


class LoggingFilter @Inject() (implicit val mat: Materializer, ec: ExecutionContext)
extends Filter with WithLogger {

  def apply(nextFilter: RequestHeader => Future[Result])
           (requestHeader: RequestHeader): Future[Result] = {

   // logger.debug(s"Request: [${requestHeader.method}] ${requestHeader.uri} ")
   nextFilter(requestHeader).map { result =>

      val location = result.header.headers.get("Location").map( l => s" >> $l").getOrElse("")
      if(! requestHeader.uri.startsWith("/assets") ){
         logger.debug(s"Response: [${requestHeader.method}] ${requestHeader.uri} => ${result.header.status}${location}")
         }

      result
    }
  }
}
