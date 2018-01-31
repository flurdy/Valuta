package controllers

import java.time.LocalDateTime
import java.time.format.DateTimeFormatter
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
}

@Singleton
class RateController @Inject() (cc: ControllerComponents, rateService: RateService)(implicit ec: ExecutionContext, rateRepository: RateRepository, apiProviderLookup: ApiProviderLookup, providerConfiguration: ApiProviderConfiguration)
extends AbstractController(cc) with I18nSupport with RateHelper with WithLogger {

   val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm")

   def list() = Action.async { implicit request =>
      for {
         rates    <- rateService.findRates()
         divisors <- rateService.findDivisors(rates)
      } yield {
         val cryptos = rates.rates.keys
                                  .toList
                                  .filter(_.isCryptoCurrency)
                                  .sortWith( _.entryName < _.entryName  )
         val fiats = rates.rates.keys
                                .toList
                                .filter(_.isFiatCurrency)
                                .sortWith( _.entryName < _.entryName  )
         val lastFormatted = rates.rates.values.toList
                        .map(_.rates.map(_._1))
                        .flatten
                        .sortWith( _.isAfter(_) )
                        .headOption
                        .map(formatter.format(_))
         Ok(views.html.rates.list(cryptos, fiats, rates, divisors, lastFormatted))
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
            logger.debug("Entering new rate")
            val rate = rateEntry.copy(source=Some(RateSource.Manual))

            rate.convertInverseAndConvert()
                .map( rate :: _ )
                .map( _.toSet )
                .map( _.map( _.save() ) )
                .map( Future.sequence(_) )
                .flatten
                .map{ _ =>
                   Redirect(routes.RateController.showEnterRates())
                      .flashing("messageSuccess"->"Rate entered")
                }
         }
      )
   }

   def showCurrencyRates(currencyCode: String) = Action.async { implicit request =>
      Currency.withNameOption(currencyCode) match {
         case Some(currency) =>
            currency.findDivisorsUsed() flatMap { divisorsUsed =>
               currency.findRatesByDates() map { dateRates =>
                  val sources = RateSource.values.toList
                  Ok(views.html.rates.currency(
                        currency, dateRates,
                        divisorsUsed,
                        currency.findDivisorsWithSources,
                        currency.findDivisorsPossible,
                        sources))
               }
            }
         case _ => Future.successful(NotFound("Currency not found"))
      }
   }

   def fetchRate(dividenName: String, divisorName: String) = Action.async { implicit request =>
      (for{
         dividen <- Currency.withNameOption(dividenName)
         divisor <- Currency.withNameOption(divisorName)
      } yield RatePair(dividen,divisor))
         .fold{
            logger.info(s"Currency invalid $dividenName and $divisorName" )
            Future.successful{
               Redirect(routes.RateController.showCurrencyRates(dividenName))
                  .flashing("messageError" -> "Currency pair invalid")
            }
         }{ ratePair =>
            ratePair.fetchRate() flatMap { rate =>
               rate.fold{
                  logger.warn("Fetch rate form problem")
                  Future.successful{
                     Redirect(routes.RateController.showCurrencyRates(dividenName))
                        .flashing("messageWarning" -> "Rate unavailable")
                  }
               }{ rateFound =>
                  logger.debug(s"Rate found $rateFound")
                  rateFound.convertInverseAndConvert()
                           .map( rateFound :: _ )
                           .map( _.toSet )
                           .map( _.map( _.save() ) )
                           .map( Future.sequence(_) )
                           .flatten
                           .map{ _ =>
                               Redirect(routes.RateController.showCurrencyRates(dividenName))
                                  .flashing("messageSuccess" -> "New rate fetched")
                           }
               }
            }
         }
   }

   def showFetchRates() = Action { implicit request =>
      Ok(views.html.rates.fetch())
   }

   def fetchAllRates() = Action.async { implicit request =>
      rateService.fetchAllRates()
         .map{ _ =>
            Redirect(routes.RateController.list())
               .flashing("messageSuccess" -> "New rates fetched")
         }
   }

}
