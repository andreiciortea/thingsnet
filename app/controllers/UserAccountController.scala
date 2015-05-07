package controllers

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc._
import play.api.libs.json._
import play.api.libs.functional.syntax._

import models._
import utils._
import services._


object UserAccountController extends Controller {

  /**
   *  Queries the repo to return the account of a given agent URI (if any).
   */
  def getAccountForAgent(agentUri: String): Future[Option[String]] = {
      ResourceService.queryForResults(UserAccount.queryAccountByHolder(agentUri)).map {
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
  
  /**
   * Create a user account (JSON and Turtle).
   */
  
  def createUserAccountFromJSON(webId: String, payload: String) = {
    implicit val createUserAccountReads = (
      (__ \ 'socialThingClass).read[String] and
      (__ \ 'socialThingOwner).read[String] and
      (__ \ 'displayedName).read[String] and
      (__ \ 'description).readNullable[String]) tupled
    
    Json.parse(payload).validate[(String, String, String, Option[String])].map {
          case (socialThingClass, socialThingOwner, displayedName, description) => {
            
            import java.net.URI
            val account = UserAccount(SmartThing(webId, socialThingClass, socialThingOwner), 
                new URI(NodeService.getPlatformURI), displayedName, description)
            
            if (!ResourceService.ask(UserAccount.queryHolderExists(webId))) {
              ResourceService.createResource(account)
              Created(account.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
            } else {
              Forbidden("There already exists an account held by " + webId + ".\n")
            }
            
          }
        }.recoverTotal {
          e => {
            BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
          }
        }
  }
  
  def createUserAccountFromTurtle(webId: Option[String], payload: String) = {
    // TODO: if the requesting agent did not send a WebID, one should be generated
    val account = UserAccount.parseTurtleString(payload, webId)
    ResourceService.createResource(account)
    Created(account.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
  }
  
  def createUserAccount = {
    Action(parse.tolerantText) { request =>
      
      val webId = request.headers.get(NodeService.HEADER_WebID)
      
      if (webId.isEmpty) {
        // TODO accept and generate webid
        Unauthorized
      } else {
        if (request.contentType.get == "application/json" || request.contentType.get == "text/json") {
          createUserAccountFromJSON(webId.get, request.body)
        } else if (request.contentType.get == "text/turtle") {
          createUserAccountFromTurtle(webId, request.body)
        } else {
          BadRequest(NodeService.BAD_REQUEST_Unknown_content_type)
        }
      }
    }
  }
  
  /**
   * Returns a user account by local id.
   */
  def getUserAccount(id: String) = Action.async { request =>
    
    if (request.headers.get(NodeService.HEADER_WebID).isEmpty) {
      Future { Unauthorized }
    } else {
      val fAccountTurtle = 
        ResourceService.getResource(NodeService.genResourceURI(NodeService.userAccountContainer, id))
      
      fAccountTurtle map{ s =>
        if (!s.isEmpty) {
            Ok(s.get).withHeaders( (CONTENT_TYPE, "text/turtle") )
        } else {
          NotFound
        }
      }
    }
  }
  
  /**
   * Returns a user account (if any) for a given agent URI.
   */
  def getUserAccountForAgent(agentUri: String) = Action.async { request =>
    if (Validator.isValidUri(agentUri)) {
      // Retrieve account by agent URI
      getAccountForAgent(agentUri).flatMap {
        accountUri => {
          if (accountUri.isEmpty) {
            // No account has been found for this agent URI
            Future { NotFound }
          } else {
            ResourceService.getResource(accountUri.get).map{ s =>
              Ok(s.get).withHeaders( (CONTENT_TYPE, "text/turtle") )
            }
          }
        }
      }
    } else {
      // Invalid or missing URI
      Future { BadRequest }
    }
  }
  
  /**
   * Delete user account.  
   */
  def deleteUserAccount = {
    Action.async { request =>
      val webId = request.headers.get(NodeService.HEADER_WebID)
      if (webId.isEmpty) {
        Future { Unauthorized }
      } else {
        getAccountForAgent(webId.get) map {
          accountUri => {
            if (accountUri.isEmpty) {
              NotFound
            } else {
              ResourceService.deleteResource(accountUri.get)
              Ok
            }
          }
        }
      }
    }
  }
}