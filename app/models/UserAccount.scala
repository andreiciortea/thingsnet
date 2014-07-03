package models

import org.w3.banana._
import org.w3.banana.binder._

import scala.util._

import repos.RDFRepositoryFactory.RDFResourceBinder


// TODO: generalize holder to Agent.
case class UserAccount(holder: SmartThing, displayedName: String, description: Option[String]) extends Resource

object UserAccountBinder extends RDFResourceBinder {
  import Ops._
  import RecordBinder._
  
  val stn = STNPrefix[Rdf]
  
  val clazz = stn.UserAccount
  implicit val classUris = classUrisFor[UserAccount](clazz)

//  import PersonBinder._
//  val personHolder = property[Person](stn.heldBy)
  
  import SmartThingBinder._
  val smartThingHolder = property[SmartThing](stn.heldBy)
  
  val displayedName = property[String](stn.name)
  val description = property[Option[String]](stn.description)

  implicit val userAccountbinder = 
    pgbWithId[UserAccount](t => URI(t.uri))
      .apply(smartThingHolder, displayedName, description)(UserAccount.apply, UserAccount.unapply) withClasses classUris
}
