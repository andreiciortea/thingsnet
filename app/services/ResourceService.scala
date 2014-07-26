package services

import org.w3.banana._
import org.w3.banana.jena._

import models.Resource
import models.{UserAccount, UserAccountBinder}
import repos.RDFRepositoryFactory


object ResourceService {
  
  val repo = RDFRepositoryFactory.makeRDFRepository
  
  def createResource(resource: Resource) = {
    resource match {
      case account: UserAccount => {
          repo.createRDFResource(account.getURI, UserAccountBinder.userAccountBinder.toPG(account))
        }
      case _ => throw new ClassCastException
    }
  }
  
  def getResource(uri: String) = {
    repo.getRDFResource(uri)
  }
  
  def deleteResource(uri: String) = {
    repo.deleteRDFResource(uri)
  }
}
