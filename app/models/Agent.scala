package models

import scala.util._

import org.w3.banana._
import org.w3.banana.binder._

import reflect.ClassTag

import repos.RDFRepositoryFactory.RDFResourceBinder


sealed class Agent(override val uri: String) extends Resource

case class Person(override val uri: String) extends Agent(uri)

case class SmartThing(override val uri: String) extends Agent(uri)



trait RDFAgentBinder extends RDFResourceBinder {
  
  val stn = STNPrefix[Rdf]
  
  implicit val from: FromURI[Rdf, Rdf#URI] = new FromURI[Rdf, Rdf#URI] {
    def fromURI(uri: Rdf#URI) = {
      FromURI.URIFromURI[Rdf].fromURI(uri)
    }
  }  
}


object AgentBinder extends RDFAgentBinder {
  import Ops._
  import RecordBinder._

  val clazz = stn.Agent
  implicit val classUris = classUrisFor[Agent](clazz)

  implicit val agentBinder: PGBinder[Rdf, Agent] = new PGBinder[Rdf, Agent] {
    
    def toPG(agent: Agent): PointedGraph[Rdf] = {
      PointedGraph[Rdf](URI(agent.uri), Graph.empty)
    }
    
    def fromPG(pointed: PointedGraph[Rdf]): Try[Agent] = {
      FromNode.FromURIFromNode[Rdf, Rdf#URI].fromNode(pointed.pointer) map {
        uri => new Agent( fromUri(uri) )
      }
    }
  } withClasses classUris
}


object PersonBinder extends RDFAgentBinder {
  import Ops._
  import RecordBinder._

  val clazz = stn.Person
  implicit val classUris = classUrisFor[Person](clazz)

  implicit val personBinder: PGBinder[Rdf, Person] = new PGBinder[Rdf, Person] {
    
    def toPG(person: Person): PointedGraph[Rdf] = {
      PointedGraph[Rdf](URI(person.uri), Graph.empty)
    }
    
    def fromPG(pointed: PointedGraph[Rdf]): Try[Person] = {
      FromNode.FromURIFromNode[Rdf, Rdf#URI].fromNode(pointed.pointer) map {
        uri => Person( fromUri(uri) )
      }
    }
  } withClasses classUris
}


object SmartThingBinder extends RDFAgentBinder {
  import Ops._
  import RecordBinder._

  val clazz = stn.SmartThing
  implicit val classUris = classUrisFor[SmartThing](clazz)

  implicit val smartThingsBinder: PGBinder[Rdf, SmartThing] = new PGBinder[Rdf, SmartThing] {
    
    def toPG(smartThing: SmartThing): PointedGraph[Rdf] = {
      PointedGraph[Rdf](URI(smartThing.uri), Graph.empty)
    }
    
    def fromPG(pointed: PointedGraph[Rdf]): Try[SmartThing] = {
      FromNode.FromURIFromNode[Rdf, Rdf#URI].fromNode(pointed.pointer) map {
        uri => SmartThing( fromUri(uri) )
      }
    }
  } withClasses classUris
}
