package controllers

import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import models._
import services._
import repositories._


@Singleton
class HomeController @Inject()(cc: ControllerComponents)
extends AbstractController(cc) with I18nSupport {

   def index() = Action { implicit request =>
      Ok(views.html.index())
   }

}

trait RateHelper {

   def currencyRateForm = Form(
      mapping(
         "dividen" -> Currency.formField,
         "divisor" -> Currency.formField,
         "date"    -> ignored(LocalDateTime.now),
         "rate"    -> bigDecimal,
         "source"  -> optional(RateSource.formField)
      )(CurrencyRate.apply)(CurrencyRate.unapply)
   )

}

@Singleton
class RateController @Inject() (cc: ControllerComponents, rateService: RateService)(implicit ec: ExecutionContext, rateRepository: RateRepository)
extends AbstractController(cc) with I18nSupport with RateHelper with WithLogger {

   def list() = Action.async { implicit request =>
      for {
         rates    <- rateService.findRates()
         divisors <- rateService.findDivisors()
      } yield {
         logger.debug("Show rates")
         Ok(views.html.rates.list(rates, divisors))
      }
   }

   def showEnterRates() = Action { implicit request =>
      Ok(views.html.rates.enter(rateService.findCurrencies()))
   }

   def enterRate = Action.async { implicit request =>
      currencyRateForm.bindFromRequest.fold(
         errors => {
            Future.successful{
               BadRequest{
                  views.html.rates.enter(rateService.findCurrencies(),
                     Some("Not a valid crypto currency"))
               }
            }
         }, rateEntry => {
            rateEntry.enterNewRate().flatMap { _ =>
               rateEntry.inverse.fold{
                  Future.successful(())
               }{
                  _.enterNewRate()
               }.map { _ =>
                  Ok("Rate entered")
               }
            }
         }
      )
   }

   def showCurrencyRates(currencyCode: String) = Action.async { implicit request =>
      Currency.withNameOption(currencyCode) match {
         case Some(currency) =>
            currency.findDivisors() flatMap { divisors =>
               currency.findRatesByDates() map { dateRates =>
                  logger.debug("date rates " + dateRates)
                  Ok(views.html.rates.currency(currency, dateRates, divisors))
               }
            }
         case _ => Future.successful(NotFound("Currency not found"))
      }
   }

}
