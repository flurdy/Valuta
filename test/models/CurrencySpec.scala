package models

import org.mockito.Mockito._
import org.mockito.ArgumentMatchers.{eq => eqTo, _}
import org.scalatest._
import org.scalatest.concurrent.{IntegrationPatience, ScalaFutures}
import org.scalatest.mockito.MockitoSugar
import org.scalatest.prop.TableDrivenPropertyChecks
import org.scalatestplus.play._

class CurrencySpec extends PlaySpec with MockitoSugar with ScalaFutures
with TableDrivenPropertyChecks {

   def given = afterWord("given")

   trait Setup {

   }

   val cryptos =
      Table (
         ("currency", "isValid"),
         ("BTC", true),
         ("Bitcoin", false),
         ("Btc", false)
      )

   val fiats =
      Table (
         ("currency", "isValid"),
         ("USD", true),
         ("US Dollars", false),
         ("Usd", false)
      )

   "CryptoCurrency" should {
      "parse withValueOpt" when given {
         forAll(cryptos) { (currency, isValid) =>
            s"$currency as $isValid" in new Setup {

               CryptoCurrency.withValueOpt(currency).isDefined mustBe isValid

            }
         }
      }
   }

   "FiatCurrency" should {
      "parse withValueOpt" when given {
         forAll(fiats) { (currency, isValid) =>
            s"$currency as $isValid" in new Setup {

               FiatCurrency.withValueOpt(currency).isDefined mustBe isValid

            }
         }
      }
   }

}
