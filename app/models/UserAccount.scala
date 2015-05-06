package models

import org.w3.banana._
import org.w3.banana.binder._
import org.w3.banana.diesel._

import scala.util._

import services.NodeService
import repos.RDFRepositoryFactory.RDFResourceDependencies

import java.net.URI


class TNURIBinder[Rdf <: RDF]()(implicit ops: RDFOps[Rdf]) extends URIBinder[Rdf, URI] {
  
  def fromURI(uri: Rdf#URI) = Success(new URI(uri.toString))
  
  def toURI(jUri: URI) = ToURI.javaURIToURI[Rdf].toURI(jUri)
}


case class UserAccount(holder: Agent, platform: URI, displayedName: String, description: Option[String], connections: Set[URI] = Set.empty) extends Resource {

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
  val platform = property[URI](stn.hostedBy)
  val connections = set[URI](stn.connectedTo)
  
  implicit val userAccountBinder = 
    pgbWithId[UserAccount](t => Ops.URI(t.getURI))
      .apply(holder, platform, displayedName, description, connections)(UserAccount.apply, UserAccount.unapply) withClasses classUris
  
  
  import scala.concurrent.ExecutionContext.Implicits.global
  
  // TODO: proper validation of Turtle string
  def parseTurtleString(ttlStr: String, requestWebId: Option[String]): UserAccount = {
    val graph = TurtleReader.read(ttlStr, "") getOrElse sys.error("Couldn't read Turtle payload.")
    val sparqlEngine = SparqlGraph(graph)
    
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |
                |SELECT ?owner ?displayedName ?clz ?description
                |WHERE { 
                |  <> rdf:type :UserAccount .
                |  <> :name ?displayedName .
                |  <> :heldBy [ :ownedBy ?owner ] .
                |  OPTIONAL { <> :heldBy [ a ?clz ] } .
                |  OPTIONAL { <> :description ?description }
                |}""".stripMargin
    
    
    import SparqlOps._
    
    val row = sparqlEngine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)
    
    val owner = Some(row("owner") getOrElse sys.error("") toString())
    
    val displayedName = row("displayedName").flatMap(_.as[String]) getOrElse sys.error("") toString()
    
    val clz =
      if (row.contains("clz")) {
        Some(row("clz") getOrElse sys.error("") toString())
      } else {
        None
      }
    
    val description =
      if (row.contains("description")) {
        Some(row("description").flatMap(_.as[String]) getOrElse sys.error("") toString()) 
      } else {
        None 
      }
    
    if (requestWebId.isEmpty) {
      // TODO gen webid
      UserAccount(SmartThing("http://www.example.com/#thing", clz, owner), new URI(NodeService.getPlatformURI), displayedName, description)
    } else {
      UserAccount(SmartThing(requestWebId.get, clz, owner), new URI(NodeService.getPlatformURI), displayedName, description)
    }
  }
  
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
