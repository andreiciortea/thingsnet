package models

import org.w3.banana._
import org.w3.banana.binder._

import scala.util._

import services.NodeService
import repos.RDFRepositoryFactory.RDFResourceDependencies

import java.net.URI


class TNURIBinder[Rdf <: RDF]()(implicit ops: RDFOps[Rdf]) extends URIBinder[Rdf, URI] {
  
  def fromURI(uri: Rdf#URI) = Success(new URI(uri.toString))
  
  def toURI(jUri: URI) = ToURI.javaURIToURI[Rdf].toURI(jUri)
}


case class UserAccount(holder: Agent, displayedName: String, description: Option[String], connections: Set[URI] = Set.empty) extends Resource {

  def getContainer = "/users"
  
  def getURI: String = 
    NodeService.genResourceURI(container = getContainer, id = uuid)

  def toGraph = UserAccount.userAccountBinder.toPG(this)
}


object UserAccount extends RDFResourceDependencies {
  import Ops._
  import RecordBinder._
  
  val stn = STNPrefix[Rdf]
  
  val clazz = stn.UserAccount
  implicit val classUris = classUrisFor[UserAccount](clazz)
  
  import Agent._
  val holder = property[Agent](stn.heldBy)
  
  val displayedName = property[String](stn.name)
  val description = property[Option[String]](stn.description)
  
  implicit val uriBinder = new TNURIBinder[Rdf]
  val connections = set[URI](stn.connectedTo)
  
  implicit val userAccountBinder = 
    pgbWithId[UserAccount](t => Ops.URI(t.getURI))
      .apply(holder, displayedName, description, connections)(UserAccount.apply, UserAccount.unapply) withClasses classUris
  
  
  def queryAccountExists(accountUri: String) = {
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |ASK { ?accountUri rdf:type :UserAccount }
                |""".stripMargin
    
    val bindings = Map(
        "accountUri" -> Ops.URI(accountUri)
      )
    
    (query, bindings)
  }
  
  def queryHolderExists(holderUri: String) = {
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |ASK {
                |  ?accountUri rdf:type :UserAccount .
                |  ?accountUri :heldBy ?holderUri .
                |}""".stripMargin
    
    val bindings = Map(
        "holderUri" -> Ops.URI(holderUri)
      )
    
    (query, bindings)
  }
  
  def queryAccountByHolder(holderUri: String) = {
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |
                |SELECT ?accountUri
                |WHERE {
                |  ?accountUri rdf:type :UserAccount .
                |  ?accountUri :heldBy ?holderUri .
                |}""".stripMargin
    
    val bindings = Map(
        "holderUri" -> Ops.URI(holderUri)
      )
    
    (query, bindings)
  }
  
  def queryConnectionExists(fromUri: String, toUri: String) = {
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |ASK { ?fromUri :connectedTo ?toUri }
                |""".stripMargin
    
    val bindings = Map(
        "fromUri" -> Ops.URI(fromUri),
        "toUri" -> Ops.URI(toUri)
      )
    
    (query, bindings)
  }


  def addConnection(fromUri: String, toUri: String): Patch[Rdf] = 
    Patch(Graph.empty, Graph(Triple(makeUri(fromUri), stn.connectedTo, makeUri(toUri))))
  
  def removeConnection(fromUri: String, toUri: String): Patch[Rdf] = 
    Patch(Graph(Triple(makeUri(fromUri), stn.connectedTo, makeUri(toUri))), Graph.empty)
}
