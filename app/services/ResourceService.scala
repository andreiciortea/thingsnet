package services

import models.Patch
import models.Resource
import models.STNPrefix
import repos.RDFRepositoryFactory

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global


object ResourceService {
  
  val repo = RDFRepositoryFactory.makeRDFRepository
  import repo._
  
  def createResource(resource: Resource) = {
    repo.createRDFResource(resource.getURI, resource.toGraph)
  }
  
  def getResource(uri: String) = {
    repo.getRDFResource(uri)
  }
  
  def patchResource(uri: String, p: Patch[Rdf]) = {
    repo.patchRDFResource(uri, p.removed, p.added)
  }
  
  def deleteResource(uri: String) = {
    repo.deleteRDFResource(uri)
  }
  
  def queryForResults(query: (String, Map[String, Rdf#URI])) = {
    repo.runSelectQuery(query)
  }
  
  def queryForGraphs(query: (String, Map[String, Rdf#URI])) = {
    repo.runConstructQuery(query)
  }
}
