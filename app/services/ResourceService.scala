package services

import models.Patch
import models.Resource
import models.{UserAccount, UserAccountBinder}
import models.{Message, MessageBinder}
import models.STNPrefix
import repos.RDFRepositoryFactory

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global


object ResourceService {
  
  val repo = RDFRepositoryFactory.makeRDFRepository

  import repo._
  
  def createResource(resource: Resource) = {
    resource match {
      case account: UserAccount => {
        repo.createRDFResource(account.getURI, UserAccountBinder.userAccountBinder.toPG(account))
       }
      case message: Message => {
        repo.createRDFResource(message.getURI, MessageBinder.messageBinder.toPG(message))
      }
      case _ => throw new ClassCastException
    }
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
  
  def runQuery(query: (String, Map[String, Rdf#URI])) = {
    repo.runQuery(query)
  }
}
