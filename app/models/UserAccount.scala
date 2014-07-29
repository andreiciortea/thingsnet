package models

import org.w3.banana._
import org.w3.banana.binder._

import scala.util._

import services.NodeService
import repos.RDFRepositoryFactory.RDFResourceBinder

import java.net.URI


class TNURIBinder[Rdf <: RDF]()(implicit ops: RDFOps[Rdf]) extends URIBinder[Rdf, URI] {
  
  def fromURI(uri: Rdf#URI) = Success(new URI(uri.toString))
  
  def toURI(jUri: URI) = ToURI.javaURIToURI[Rdf].toURI(jUri)
}


case class UserAccount(holder: Agent, displayedName: String, description: Option[String], connections: Set[URI] = Set.empty) extends Resource {

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
  
  implicit val uriBinder = new TNURIBinder[Rdf]
  val connections = set[URI](stn.connectedTo)
  
  implicit val userAccountBinder = 
    pgbWithId[UserAccount](t => Ops.URI(t.getURI))
      .apply(holder, displayedName, description, connections)(UserAccount.apply, UserAccount.unapply) withClasses classUris
}
