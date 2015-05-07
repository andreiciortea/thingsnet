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
  
  def getResourcesInList(uriList: List[String]) = {
    repo.getGraphsInList(uriList)
  }
  
  def patchResource(uri: String, p: Patch[Rdf]) = {
    repo.patchRDFResource(uri, p.removed, p.added)
  }
  
  def deleteResource(uri: String) = {
    repo.deleteRDFResource(uri)
  }
  
  
  
  def getSTNSpec = repo.getSTNSpec
  
  def ask(query: (String, Map[String, Rdf#URI])) = repo.runAskQuery(query)
  
  def queryForResults(query: (String, Map[String, Rdf#URI])) = {
    repo.runSelectQuery(query)
  }
  
  def constructGraphs(query: (String, Map[String, Rdf#URI])) = {
    repo.runConstructQuery(query)
  }
  
  
  import play.api.libs.json._
  
  def queryForOne(query: (String, Map[String, Rdf#URI])): Future[Option[String]] = {
    repo.runSelectQuery(query) map {
      resultsJson => {
        val results = (Json.parse(resultsJson) \\ "value")
        if (results.isEmpty) {
            None
        } else {
          results(0).asOpt[String]
        }
      }
    }
  }
  
  def queryForListOfOne(query: (String, Map[String, Rdf#URI])): Future[Seq[String]] = {
    repo.runSelectQuery(query) map {
      resultsJson => (Json.parse(resultsJson) \\ "value").flatMap(_.asOpt[String])
    }
  }
}
