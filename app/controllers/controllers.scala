package controllers

import java.time.LocalDateTime
import javax.inject.{Inject, Singleton}
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.data.validation.Constraints._
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
         "ratePair" -> mapping(
            "dividen" -> Currency.formField,
            "divisor" -> Currency.formField
         )(RatePair.apply)(RatePair.unapply),
         "date"    -> ignored(LocalDateTime.now),
         "rate"    -> bigDecimal,
         "source"  -> optional(RateSource.formField)
      )(CurrencyRate.apply)(CurrencyRate.unapply)
   )

   def fetchRateForm = Form (
      mapping (
         "dividen" -> Currency.formField,
         "divisor" -> Currency.formField
      )(RatePair.apply)(RatePair.unapply)
   )
}

@Singleton
class RateController @Inject() (cc: ControllerComponents, rateService: RateService)(implicit ec: ExecutionContext, rateRepository: RateRepository, apiProvider: ApiProvider)
extends AbstractController(cc) with I18nSupport with RateHelper with WithLogger {

   def list() = Action.async { implicit request =>
      for {
         rates    <- rateService.findRates()
         divisors <- rateService.findDivisors()
      } yield Ok(views.html.rates.list(rates, divisors))
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
            logger.debug("Entering new rate")
            rateEntry.copy(source=Some(RateSource.Manual))
                     .enterNewRate().flatMap {
                        _.inverse.fold{
                           Future.successful(rateEntry)
                        }( _.enterNewRate() )
                        .map { _ =>
                           Redirect(routes.RateController.showEnterRates())
                              .flashing("messageSuccess"->"Rate entered")
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
                  val sources = RateSource.values.toList
                  Ok(views.html.rates.currency(currency, dateRates, divisors, Currency.divisorCurrencies.toList, sources))
               }
            }
         case _ => Future.successful(NotFound("Currency not found"))
      }
   }

   def fetchRate(dividen: String) = Action.async { implicit request =>
      fetchRateForm.bindFromRequest.fold(
         errors => {
            Future.successful{
               Redirect(routes.RateController.showCurrencyRates(dividen)).flashing("messageError" -> "Problem fetching new rate")
            }
         }, ratePair => {
            logger.debug("Fetch new rate")

            ratePair.fetchRate() map { rate =>
               Redirect(routes.RateController.showCurrencyRates(dividen)).flashing("messageSuccess" -> "New rate fetched")
            }
         }
      )
   }

}
