package controllers

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import models._
import utils._
import services._

import java.net.URI


object ConnectionsController extends Controller {

  /**
   * Create connection to a registered or external agent.
   */
  
  def addConnection(webId: String, targetUri: String): Future[Result] = {
    UserAccountController.getUserAccountUriForAgent(webId).flatMap {
      sourceAccountUri => {
        if (sourceAccountUri.isEmpty) {
            Future { Forbidden("You need to create an account.") }
        } else {
          if (ResourceService.ask(UserAccount.queryAccountExists(targetUri))) {
            ResourceService.patchResource(sourceAccountUri.get, 
                UserAccount.addConnection(sourceAccountUri.get, targetUri))
            Future { Created }
          } else {
            // Check if the target URI is a registered agent.
            UserAccountController.getUserAccountUriForAgent(targetUri).map {
              targetAccountUri => {
                if (!targetAccountUri.isEmpty) {
                  ResourceService.patchResource(sourceAccountUri.get, 
                      UserAccount.addConnection(sourceAccountUri.get, targetAccountUri.get))
                  Created
                } else {
                  // An external target URI.
                  // TODO: follow the URI and validate that it is for an agent or user account.
                  ResourceService.patchResource(sourceAccountUri.get, 
                      UserAccount.addConnection(sourceAccountUri.get, targetUri))
                  Created
                }
              }
            }
          }
        }
      }
    }
  }
  
  def extractConnectionTargetFromJSON(payload: String): Option[String] = {
    implicit val connectionReads = ((__ \ 'targetUri).read[String])

    Json.parse(payload).validate[(String)].map {
      case (targetUri) => Some(targetUri)
    }.recoverTotal {
      e => {
        None
      }
    }
  }
  
  def extractConnectionTargetFromTurtle(payload: String): Option[String] = {
    TurtleParser.getOne(payload, 
        "<> stn:connectedTo ?targetUri .",
        "targetUri")
  }
  
  def createConnection = {
    Action.async(parse.tolerantText) { request =>
      val webId = request.headers.get(NodeService.HEADER_WebID)
      
      if (webId.isEmpty) {
        // TODO accept and generate webid
        Future { Unauthorized }
      } else {
        val targetUri = {
          if (request.contentType.get == "application/json" || request.contentType.get == "text/json") {
            extractConnectionTargetFromJSON(request.body)
          } else if (request.contentType.get == "text/turtle") {
            extractConnectionTargetFromTurtle(request.body)
          } else {
            None
          }
        }
        // TODO: problems with url validation
//        if (targetUri.isEmpty || !Validator.isValidUri(targetUri.get) || webId.get == targetUri.get) {
        if (targetUri.isEmpty || webId.get == targetUri.get) {
          Future { BadRequest }
        } else {
          addConnection(webId.get, targetUri.get)
        }
      }
    }
  }
  
  /**
   * Remove a connection.
   */
  
  def removeConnection(webId: String, targetUri: String): Future[Result] = {
    UserAccountController.getUserAccountUriForAgent(webId).flatMap {
      sourceAccountUri => {
        if (sourceAccountUri.isEmpty) {
          Future { Forbidden("You need to create an account.") }
        } else {
          if (ResourceService.ask(UserAccount
              .queryConnectionExists(sourceAccountUri.get, targetUri))) {
            
            ResourceService.patchResource(sourceAccountUri.get, 
                UserAccount.removeConnection(sourceAccountUri.get, targetUri))
            
            Future { Ok }
          } else {
            UserAccountController.getUserAccountUriForAgent(targetUri).map {
              targetAccountUri => 
                if (ResourceService.ask(UserAccount
                    .queryConnectionExists(sourceAccountUri.get, targetAccountUri.get))) {
                  
                  ResourceService.patchResource(sourceAccountUri.get, 
                      UserAccount.removeConnection(sourceAccountUri.get, targetAccountUri.get))
                  
                  Ok
                } else {
                  NotFound
                }
            }.recover {
              case e: Exception => NotFound
            }
          }
        }
      }
    }
  }
  
  def deleteConnection(targetUri: String) = {
    Action.async(parse.tolerantText) { request =>
      val webId = request.headers.get(NodeService.HEADER_WebID)
      
      if (webId.isEmpty) {
        Future { Unauthorized }
      } else {
        if (!Validator.isValidUri(targetUri)) {
          Future { BadRequest }
        } else {
          removeConnection(webId.get, targetUri)
        }
      }
    }
  }
  
  /**
   * Get outgoing connections for a given user account.
   */
  
  def getConnections(accountUri: String) = {
    Action.async { request =>
      val webId = request.headers.get(NodeService.HEADER_WebID)
      
      if (webId.isEmpty) {
        Future { Unauthorized }
      } else {
        // Validation not working properly for this URI
//        if (!Validator.isValidUri(accountUri)) {
//          Future { BadRequest }
//        } else {
          if (!ResourceService.ask(UserAccount.queryAccountExists(accountUri))) {
            Future { NotFound }
          } else {
            val query = 
              if (request.uri.contains("out")) UserAccount.queryGetConnections(Some(accountUri), None)
              else UserAccount.queryGetConnections(None, Some(accountUri))
            ResourceService
              .constructGraphs(query) map {
                result => Ok(result).withHeaders(("Content-Type", "text/turtle"))
            }
          }
//        }
      }
    }
  }
  
  def getInConnections(accountUri: String) = {
    Action.async { request =>
      val webId = request.headers.get(NodeService.HEADER_WebID)
      
      if (webId.isEmpty) {
        Future { Unauthorized }
      } else {
        // Validation not working properly for this URI
//        if (!Validator.isValidUri(accountUri)) {
//          Future { BadRequest }
//        } else {
          if (!ResourceService.ask(UserAccount.queryAccountExists(accountUri))) {
            Future { NotFound }
          } else {
            ResourceService
              .constructGraphs(UserAccount.queryGetConnections(None, Some(accountUri))) map {
                result => Ok(result).withHeaders(("Content-Type", "text/turtle"))
            }
          }
//        }
      }
    }
  }
  
}