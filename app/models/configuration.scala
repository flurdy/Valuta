package models

import com.google.inject.ImplementedBy
import enumeratum._
import enumeratum.values._
import enumeratum.EnumEntry._
import java.net.URI
import util.Try
import javax.inject.{Inject, Singleton}
import play.api.Configuration

@ImplementedBy(classOf[DefaultApplicationConfiguration])
trait ApplicationConfiguration extends WithLogger {

   def prefix: String

   def configuration: Configuration

   lazy val rootConfig = configuration.get[Configuration](prefix)

   def missingConfiguration(property: String) = throw new IllegalStateException(s"No configuration for $property")

   def findConfig(property: String): Option[Configuration] =
      rootConfig.getOptional[Configuration](property)

   def findString(property: String): Option[String] =
      rootConfig.getOptional[String](property)

   def findInt(property: String): Option[Int] =
      rootConfig.getOptional[Int](property)

   def findBoolean(property: String): Option[Boolean] =
      rootConfig.getOptional[Boolean](property)

   def getConfig(property: String): Configuration =
      findConfig(property)
         .getOrElse(missingConfiguration(property))

   def getString(property: String): String =
      findString(property)
         .getOrElse(missingConfiguration(property))

   def getInt(property: String): Int =
      findInt(property)
         .getOrElse(missingConfiguration(property))

   def getBoolean(property: String): Boolean =
      findBoolean(property)
         .getOrElse(missingConfiguration(property))

   def isEnabled(property: String, default: Boolean = false): Boolean =
      findBoolean(s"${property}.enabled")
         .fold( default )( maybe => maybe )

   def findSubKeys(property: String): Set[String] =
      findConfig(property).fold[Set[String]](Set())(_.subKeys)

}

@Singleton
class DefaultApplicationConfiguration @Inject() (val configuration: Configuration) extends ApplicationConfiguration {
   override val prefix = "com.flurdy.valuta"
}


@ImplementedBy(classOf[DefaultDatabaseConfiguration])
trait DatabaseConfiguration extends ApplicationConfiguration {

   def redisUrl: Option[URI] =
      findString("redis.url").flatMap { urlFound =>
         Try {
            new URI(urlFound)
         }.toOption
      }

   def redisHost: String =
      redisUrl.map( _.getHost )
               .orElse( findString("redis.host") )
               .getOrElse( throw new IllegalStateException("No redis host configured"))

   def redisPort: Int =
      redisUrl.map( _.getPort )
               .orElse( findInt("redis.port") )
               .getOrElse( throw new IllegalStateException("No redis port configured"))

   private def redisUrlAuth(uri: URI): Option[(String,String)] =
      Option(uri.getUserInfo)
         .flatMap{ userInfo =>
            userInfo.split(":").toList match {
               case user :: pw :: _ => Some(user,pw)
               case _ => None
            }
         }

   def redisUser: Option[String] =
      redisUrl.fold{
            findString("redis.user")
         }( redisUrlAuth(_).map(_._1) )

   def redisPassword: Option[String] =
      redisUrl.fold{
            findString("redis.password")
         }( redisUrlAuth(_).map(_._2) )
}


@Singleton
class DefaultDatabaseConfiguration @Inject() (val configuration: Configuration)
extends DatabaseConfiguration {
   override val prefix = "database"
}


@ImplementedBy(classOf[DefaultApiProviderConfiguration])
trait ApiProviderConfiguration extends WithLogger {

   def appConfig: ApplicationConfiguration

   lazy val providerConfig: ApplicationConfiguration =
      if( appConfig.isEnabled("provider.cryptowatch") )
         new ApplicationConfiguration {
            val prefix = "provider.cryptowatch.pair"
            val configuration = appConfig.rootConfig
         }
      else
         throw new IllegalStateException(
            s"Configuration missing or disabled for provider.cryptowatch")

   def fixerIoApiKey: Option[String] = appConfig.findString("provider.fixerio.apikey")

   private def toDividenProperty(dividen: Currency) = s"${dividen.entryName.toLowerCase}"

   private def toPairProperty(pair: RatePair) =
       s"${toDividenProperty(pair.dividen)}.${pair.divisor.entryName.toLowerCase}"

   private def toSourceProperty(pair: RatePair, source: RateSource) =
       s"${toPairProperty(pair)}.source.${source.entryName.toLowerCase}"

   private def isDividenEnabled(dividen: Currency) =
      providerConfig.isEnabled(toDividenProperty(dividen))

   private def isPairEnabled(pair: RatePair) = providerConfig.isEnabled(toPairProperty(pair))

   private def isSourceEnabled(pair: RatePair, source: RateSource) =
      providerConfig.isEnabled(toSourceProperty(pair, source))

   def findSources(pair: RatePair): List[RateSource] =
      providerConfig.findSubKeys(s"${toPairProperty(pair)}.source")
                    .toList
                    .map( source => RateSource.withNameOption(source.toLowerCase) )
                    .flatten
                    .filter( source => isSourceEnabled(pair, source) )

   private def hasSources(pair: RatePair) = ! findSources(pair).isEmpty

   def findRateUrl(pair: RatePair): Option[RatePairSource] =
      if( isDividenEnabled(pair.dividen)
         && isPairEnabled(pair)
            && hasSources(pair) ){
               val source = findSources(pair).head
               providerConfig.findString(s"${toSourceProperty(pair, source)}.url")
                             .map( RatePairSource( source, pair, _) )
     } else None

   def findDivisors(dividen: Currency): List[Currency] = {
      if( isDividenEnabled(dividen) ){
         providerConfig.findSubKeys(toDividenProperty(dividen))
               .toList
               .filter( _ != "enabled" )
               .map( divisor => Currency.withNameOption(divisor.toUpperCase) )
               .flatten
               .filter( divisor => isPairEnabled(RatePair(dividen,divisor)) )
      } else {
         // logger.warn(s"No dividen config or disabled for $dividen")
         List()
      }
   }

   def findDivisorsWithSources(dividen: Currency): List[Currency] =
      findDivisors(dividen)
         .filter(divisor =>
            hasSources( RatePair(dividen, divisor) ) )

}

@Singleton
class DefaultApiProviderConfiguration @Inject() (val appConfig: ApplicationConfiguration) extends ApiProviderConfiguration


sealed abstract class FeatureToggle extends EnumEntry with Lowercase

case object FeatureToggle extends PlayEnum[FeatureToggle]{
   val values = findValues
   case object EnterRate extends FeatureToggle
}

@ImplementedBy(classOf[DefaultFeatureToggles])
trait FeatureToggles {

   def appConfig: ApplicationConfiguration

   def isEnabled(feature: FeatureToggle) = appConfig.isEnabled(s"feature.${feature.entryName.toLowerCase}")

}

@Singleton
class DefaultFeatureToggles @Inject() (val appConfig: ApplicationConfiguration) extends FeatureToggles
