package models

import org.w3.banana._

import repos.RDFRepositoryFactory.RDFResourceDependencies
import services.NodeService

abstract class Resource extends RDFResourceDependencies {

  val uuid = NodeService.genUUID.toString
  
  def getContainer: String
  
  def getURI: String
  
  def toGraph: PointedGraph[Rdf]
  
  def toTurtle: String = 
    TurtleWriter.asString(toGraph.graph, "")  getOrElse sys.error("Couldn't serialize the resource graph.")
}
