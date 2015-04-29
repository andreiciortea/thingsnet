package models

import scala.util._

import org.w3.banana._
import org.w3.banana.binder._
import org.w3.banana.diesel._

import repos.RDFRepositoryFactory.RDFResourceDependencies


sealed class Agent(uri: String) extends Resource {
  
  def getContainer = ""
  
  def getURI = uri
  
  def toGraph = Agent.agentBinder.toPG(this)
}

case class Person(uri: String) extends Agent(uri)

case class SmartThing(uri: String, clz: Option[String], owner: Option[String]) extends Agent(uri)



object Agent extends RDFResourceDependencies {
  
  def apply(uri: String) = new Agent(uri)
  
  import Ops._
  val stn = STNPrefix[Rdf]
  
  implicit val from: FromURI[Rdf, Rdf#URI] = new FromURI[Rdf, Rdf#URI] {
    def fromURI(uri: Rdf#URI) = {
      FromURI.URIFromURI[Rdf].fromURI(uri)
    }
  }
  
  implicit val agentBinder: PGBinder[Rdf, Agent] = new PGBinder[Rdf, Agent] {
    
    def toPG(agent: Agent): PointedGraph[Rdf] = {
      val pointed = PointedGraph[Rdf](URI(agent.getURI), Graph.empty)
      agent match {
        case Person(_) => pointed.a(stn.Person)
        case SmartThing(_,clz,owner) => {
          val pg = if (clz.isEmpty) pointed.a(stn.SmartThing) else pointed.a(makeUri(clz.get))
          if (owner.isEmpty) {
            pg 
          } else {
            PointedGraph(pg.pointer,
                pg.graph union Graph(  Triple(pg.pointer, stn.ownedBy, makeUri(owner.get))  )
            )
          }
        }
        case _ => pointed.a(stn.Agent)
      }
    }
    
    def fromPG(pointed: PointedGraph[Rdf]): Try[Agent] = {
      FromNode.FromURIFromNode[Rdf, Rdf#URI].fromNode(pointed.pointer) map {
        uri => {
          if (pointed.pointer.isA(stn.Agent)) {
            new Agent( fromUri(uri) )
          } else if (pointed.pointer.isA(stn.Person)) {
            new Person( fromUri(uri) )
          } else if (pointed.pointer.isA(stn.SmartThing)) {
            new SmartThing( fromUri(uri), Some(pointed.pointer.getName), Some(pointed.pointer.getName) ) // TOOD: wtf?!
          } else throw new ClassCastException
        }
      }
    }
  }
}
