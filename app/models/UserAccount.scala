package models

import org.w3.banana._
import org.w3.banana.binder._

import scala.util._

import services.NodeService
import repos.RDFRepositoryFactory.RDFResourceBinder


case class UserAccount(holder: Agent, displayedName: String, description: Option[String]) extends Resource {

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
  
  import AgentBinder._
  val holder = property[Agent](stn.heldBy)
  val displayedName = property[String](stn.name)
  val description = property[Option[String]](stn.description)

  implicit val userAccountBinder = 
    pgbWithId[UserAccount](t => URI(t.getURI))
      .apply(holder, displayedName, description)(UserAccount.apply, UserAccount.unapply) withClasses classUris
}
