package models

import com.google.inject.ImplementedBy
import javax.inject.{Inject, Singleton}
import play.api.Configuration

@ImplementedBy(classOf[DefaultApplicationConfiguration])
trait ApplicationConfiguration {

   def rootConfig: Configuration

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
      findBoolean(property)
         .orElse( findBoolean(s"${property}.enabled") )
         .fold( default )( maybe => maybe )

}

@Singleton
class DefaultApplicationConfiguration @Inject() (val configuration: Configuration) extends ApplicationConfiguration {
   override lazy val rootConfig = configuration.get[Configuration]("com.flurdy.valuta")
}


@ImplementedBy(classOf[DefaultDatabaseConfiguration])
trait DatabaseConfiguration extends ApplicationConfiguration {

   def redisHost: String = findString("database.redis.host").getOrElse("localhost")

   def redisPort: Int = findInt("database.redis.port").getOrElse(6379)

}


@Singleton
class DefaultDatabaseConfiguration @Inject() (val configuration: Configuration) extends DatabaseConfiguration {
   override lazy val rootConfig = configuration
}


@ImplementedBy(classOf[DefaultApiProviderConfiguration])
trait ApiProviderConfiguration {

   def appConfig: ApplicationConfiguration

   lazy val pairConfig: ApplicationConfiguration =
      if( appConfig.isEnabled("provider.cryptowatch") )
         new ApplicationConfiguration {
            val rootConfig = appConfig.getConfig("provider.cryptowatch.pair")
         }
      else
         throw new IllegalStateException(
            s"Configuration missing or disabled for provider.cryptowatch")   

   def findRateUrl(pair: RatePair): Option[RatePairSource] = {
      val source = RateSource.Gdax
      val sourceProperty = s"${pair.dividen}.${pair.divisor}.source.${source}"
      if(pairConfig.isEnabled(s"${sourceProperty}") )
         pairConfig.findString(s"$sourceProperty.url" )
            .map{ url =>
               RatePairSource( source, pair, url )
            }
      else
         throw new IllegalStateException(
            s"Configuration missing or disabled for $sourceProperty")
   }

}

@Singleton
class DefaultApiProviderConfiguration @Inject() (val appConfig: ApplicationConfiguration) extends ApiProviderConfiguration
