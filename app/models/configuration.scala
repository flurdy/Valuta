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
