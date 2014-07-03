package models

import org.w3.banana._
import org.w3.banana.binder._

import scala.util._

import services.NodeService
import repos.RDFRepositoryFactory.RDFResourceBinder


// TODO: generalize holder to Agent.
case class UserAccount(holder: Person, displayedName: String, description: Option[String]) extends Resource {

  def getContainer = "/users"
  
  def getURI: String = 
    NodeService.genResourceURI(container = getContainer, id = displayedName)
}

object UserAccountBinder extends RDFResourceBinder {
  import Ops._
  import RecordBinder._
  
  val stn = STNPrefix[Rdf]
  
  val clazz = stn.UserAccount
  implicit val classUris = classUrisFor[UserAccount](clazz)

//  import PersonBinder._
//  val personHolder = property[Person](stn.heldBy)
  
  import PersonBinder._
  val personHolder = property[Person](stn.heldBy)
  
  val displayedName = property[String](stn.name)
  val description = property[Option[String]](stn.description)

  implicit val userAccountBinder = 
    pgbWithId[UserAccount](t => URI(t.getURI))
      .apply(personHolder, displayedName, description)(UserAccount.apply, UserAccount.unapply) withClasses classUris
}
