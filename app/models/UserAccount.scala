package models

import play.api.libs.functional.syntax.functionalCanBuildApplicative
import play.api.libs.functional.syntax.toFunctionalBuilderOps

import org.w3.banana._
import org.w3.banana.binder._

import repos.RDFRepositoryFactory.RDFResourceBinder


case class UserAccount(holder: Person, displayedName: String, description: Option[String]) extends Resource

object UserAccountBinder extends RDFResourceBinder {
  import Ops._
  import RecordBinder._
  
  val stn = STNPrefix[Rdf]
  
  val clazz = stn.UserAccount
  implicit val classUris = classUrisFor[UserAccount](clazz)

  import PersonBinder._
  
  val holder = property[Person](stn.heldBy)
  val displayedName = property[String](stn.name)
  val description = property[Option[String]](stn.description)

  implicit val userAccountbinder = 
    pgbWithId[UserAccount](t => URI(t.uri))
      .apply(holder, displayedName, description)(UserAccount.apply, UserAccount.unapply) withClasses classUris
}
