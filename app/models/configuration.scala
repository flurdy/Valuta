package models

import com.google.inject.ImplementedBy
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

   def redisHost: String = findString("redis.host").getOrElse("localhost")

   def redisPort: Int = findInt("redis.port").getOrElse(6379)

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

   def findRateUrl(pair: RatePair): Option[RatePairSource] = {
      val source = RateSource.Gdax
      val dividenProperty = s"${pair.dividen.entryName.toLowerCase}"
      val pairProperty = s"${dividenProperty}.${pair.divisor.entryName.toLowerCase}"
      val sourceProperty = s"${pairProperty}.source.${source.entryName.toLowerCase}"
      if(providerConfig.isEnabled(dividenProperty) ) {
         if(providerConfig.isEnabled(pairProperty) ) {
            if(providerConfig.isEnabled(sourceProperty) ) {
               providerConfig.findString(s"${sourceProperty}.url")
                  .map{ url =>
                     RatePairSource( source, pair, url )
                  }
            } else {
               logger.warn(s"No source config or disabled for $sourceProperty")
               None
            }
         } else {
            logger.warn(s"No pair config or disabled for $pairProperty")
            None
         }
      } else {
         logger.warn(s"No dividen config or disabled for $dividenProperty")
         None
      }
   }

   private def hasSources(divisorProperty: String) = {
      val sourceProperty = s"${divisorProperty}.source"
      ! providerConfig.findSubKeys(sourceProperty)
                    .toList
                    .filter( source => providerConfig.isEnabled(s"${sourceProperty}.${source}"))
                    .isEmpty
   }

   def findDivisors(dividen: Currency): List[Currency] = {
      val dividenProperty = s"${dividen.entryName.toLowerCase}"
      if(providerConfig.isEnabled(dividenProperty) ) {
         providerConfig.findSubKeys(dividenProperty)
               .toList
               .filter( _ != "enabled" )
               .filter( divisor => providerConfig.isEnabled(s"${dividenProperty}.${divisor}") )
               .map( divisor => Currency.withNameOption(divisor.toUpperCase) )
               .flatten
      } else {
         logger.warn(s"No dividen config or disabled for $dividenProperty")
         List()
      }
   }

   def findDivisorsWithSources(dividen: Currency): List[Currency] =
      findDivisors(dividen)
         .filter(divisor =>
            hasSources(s"${dividen.entryName.toLowerCase}.${divisor.entryName.toLowerCase}") )


}

@Singleton
class DefaultApiProviderConfiguration @Inject() (val appConfig: ApplicationConfiguration) extends ApiProviderConfiguration
