package controllers

import javax.inject.{Inject, Singleton}
import play.api._
import play.api.data._
import play.api.data.Forms._
import play.api.i18n._
import play.api.mvc._
import scala.concurrent.{ExecutionContext, Future}
import models._
import services._
// import repositories._
// import connector._
import CryptoCurrency._
import FiatCurrency._


@Singleton
class HomeController @Inject()(cc: ControllerComponents)
extends AbstractController(cc) with I18nSupport {

   def index() = Action { implicit request =>
      Ok(views.html.index())
   }

}

trait RateHelper {
   val cryptoPerCryptoRateForm = Form(
      mapping(
         // "csrfToken" -> ignored(""),
         "dividen" -> CryptoCurrency.formField,
         "divisor" -> CryptoCurrency.formField,
         "rate" -> bigDecimal
      )(CryptoPerCryptoRate.apply)(CryptoPerCryptoRate.unapply)
   )

   val cryptoPerFiatRateForm = Form(
      mapping(
         // "csrfToken" -> ignored(""),
         "dividen" -> CryptoCurrency.formField,
         "divisor" -> FiatCurrency.formField,
         "rate" -> bigDecimal
      )(CryptoPerFiatRate.apply)(CryptoPerFiatRate.unapply)
   )

   val fiatPerFiatRateForm = Form(
      mapping(
         // "csrfToken" -> ignored(""),
         "dividen" -> FiatCurrency.formField,
         "divisor" -> FiatCurrency.formField,
         "rate" -> bigDecimal
      )(FiatPerFiatRate.apply)(FiatPerFiatRate.unapply)
   )
}

@Singleton
class RateController @Inject() (cc: ControllerComponents, rateService: RateService)(implicit ec: ExecutionContext)
extends AbstractController(cc) with I18nSupport with RateHelper with WithLogger {

   def list() = Action.async { implicit request =>
      rateService.findRates.map{ rates =>
         Ok(views.html.rates.list(rates))
      }
   }

   def showEnter() = Action { implicit request =>
      Ok(views.html.rates.enter(rateService.findCurrencies()))
   }

   def enterCryptoPerCryptoRate = Action.async { implicit request =>
      cryptoPerCryptoRateForm.bindFromRequest.fold(
         errors => {
            Future.successful{
               BadRequest{
                  views.html.rates.enter(rateService.findCurrencies(),
                     Some("Not a valid crypto currency"))
               }
            }
         }, rateEntry => {
            rateService.enterNewRate(rateEntry).map { _ =>
               Ok("Rate entered")
            }
         }
      )
   }

   def enterCryptoPerFiatRate = Action.async { implicit request =>
      cryptoPerFiatRateForm.bindFromRequest.fold(
         errors => {
            Future.successful{
               BadRequest(
                  views.html.rates.enter(rateService.findCurrencies(),
                  Some("Not a valid crypto currency")))
            }
         }, rateEntry => {
            rateService.enterNewRate(rateEntry).map { _ =>
               Ok("Rate entered")
            }
         }
      )
   }

   def enterFiatPerFiatRate = Action.async { implicit request =>
      fiatPerFiatRateForm.bindFromRequest.fold(
         errors => {
            Future.successful{
               BadRequest(
                  views.html.rates.enter(rateService.findCurrencies(),
                  Some("Not a valid crypto currency")))
            }
         }, rateEntry => {
            rateService.enterNewRate(rateEntry).map { _ =>
               Ok("Rate entered")
            }
         }
      )
   }

}
