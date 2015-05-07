package controllers

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.functional.syntax._
//import play.api.libs.functional.syntax.functionalCanBuildApplicative
//import play.api.libs.functional.syntax.toFunctionalBuilderOps

import models.{Agent, Person, SmartThing}
import models.UserAccount
import models.Message
import services.{NodeService, ResourceService}
import utils.Validator

import org.apache.commons.validator.routines.UrlValidator
import java.net.URI
import java.net.URLDecoder


object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def spec = Action {
    Ok(ResourceService.getSTNSpec)
  }

  
  // TODO: review returned HTTP status codes
  // TODO: refactor body parsing
  
  
  def parserTest = Action {
    import utils.Parser
    
    Ok(Parser.parse).withHeaders(("Content-Type", "text/turtle"))
  }
  
  
  def socialTV = Action.async {
    import democlient.SocialTV

//    SocialTV.run map {
//      output => Ok(output)
//    }
    
    SocialTV.run map {
      response => Ok(response)
    }
  }
  
  
  /**
   *   Connection handlers.
   */
  
  def createConnection = {
    implicit val connectionReads = (
      (__ \ 'mywebid).read[String] and
      (__ \ 'targetUri).read[String]) tupled
    
    Action.async(parse.json) { request =>
      request.body.validate[(String, String)].map {
        
        case (mywebid, targetUri) => {
          UserAccountController.getAccountForAgent(mywebid).flatMap {
            myAccountUri => {
              
//              if (ResourceService.ask(UserAccount.queryAccountExists(targetUri))) {
//              } else {
//                  BadRequest("The target of the connection is not a registered account.\n")
//              }
              
              UserAccountController.getAccountForAgent(targetUri).map {
                targetAccountUri => {
                  ResourceService.patchResource(myAccountUri.get, UserAccount.addConnection(myAccountUri.get, targetAccountUri.get))
                  Created
                }
              }.recover {
                // TODO: do some validation before adding a connection to the provided URI.
                case e: Exception => {
                  ResourceService.patchResource(myAccountUri.get, UserAccount.addConnection(myAccountUri.get, targetUri))
                  Created
                }
              }
            }
          }.recover {
            case e: Exception => Forbidden(e.getMessage)
          }
        }
      
      }.recoverTotal {
        e => future {
          if (JsError.toFlatJson(e).toString().contains("mywebid")) {
            Unauthorized
          } else {
            BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
          }
        }
      }
    }
  }
  
  def deleteConnection = {
    implicit val connectionReads = (
      (__ \ 'mywebid).read[String] and
      (__ \ 'targetUri).read[String]) tupled
    
    Action.async(parse.json) { request =>
      request.body.validate[(String, String)].map {
        case (mywebid, targetUri) => {
          UserAccountController.getAccountForAgent(mywebid).flatMap {
            myAccountUri => {
              
              // TODO: refactor this
              if (ResourceService.ask(UserAccount.queryConnectionExists(myAccountUri.get, targetUri))) {
                ResourceService.patchResource(myAccountUri.get, UserAccount.removeConnection(myAccountUri.get, targetUri))
                future { Ok }
              } else {
                UserAccountController.getAccountForAgent(targetUri).map {
                  targetAccountUri => 
                    if (ResourceService.ask(UserAccount.queryConnectionExists(myAccountUri.get, targetAccountUri.get))) {
                      ResourceService.patchResource(myAccountUri.get, UserAccount.removeConnection(myAccountUri.get, targetAccountUri.get))
                      Ok
                    } else {
                      NotFound
                    }
                }.recover {
                  case e: Exception => NotFound
                }
              }
              
            }
          }.recover {
            case e: Exception => BadRequest(e.getMessage)
          }
        }
      }.recoverTotal {
        e => future {
          if (JsError.toFlatJson(e).toString().contains("mywebid")) {
            Unauthorized
          } else {
            BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
          }
        }
      }
    }
  }
  
  /**
   *   Message handlers.
   */
  // TODO: body param should be required
  def createMessage = {
    implicit val createMessageReads = (
      (__ \ 'mywebid).read[String] and
      (__ \ 'receiver).read[String] and
      (__ \ 'replyTo).readNullable[String] and
      (__ \ 'subject).readNullable[String] and
      (__ \ 'body).readNullable[String]) tupled
    
    Action.async(parse.json) { request =>
      request.body.validate[(String, String, Option[String], Option[String], Option[String])].map {
        case (mywebid, receiver, replyTo, subject, body) => {
          UserAccountController.getAccountForAgent(mywebid).map {
            myAccountUri => {  
              val optReplyTo: Option[URI] = if (replyTo.isEmpty) None else Some(new URI(replyTo.get))
              
              val message = Message(new URI(myAccountUri.get), new URI(receiver), optReplyTo, subject, body)
              
              if (ResourceService.ask(UserAccount.queryAccountExists(receiver))) {
                ResourceService.createResource(message)
                Created(message.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
              } else {
                BadRequest("The receiver of the message is not a registered account.\n")
              }
            }
          }.recover {
            case e: Exception => BadRequest(e.getMessage)
          }
          
        }
      }.recoverTotal {
        e => future {
          BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
        }
      }
    }
  }

  def getMessages = {
    implicit val getMessagesReads = (
      (__ \ 'mywebid).read[String])
    
    Action.async(parse.json) { request =>
      request.body.validate[String].map {
        case (mywebid) => {
          println("got request")
          UserAccountController.getAccountForAgent(mywebid).flatMap {
            myAccountUri =>
              ResourceService.queryForGraphs(Message.queryMessagesForUser(myAccountUri.get)) map {
                s =>
                  Ok(s).withHeaders( (CONTENT_TYPE, "text/turtle") )
              }
          }.recover {
            case e: Exception => BadRequest(e.getMessage)
          }
        }
      }.recoverTotal {
        e => future {
          BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
        }
      }
    }
  }

  def getMessageById(id: String) = Action.async {
//    println("id = " + id)
//    println(NodeService.genResourceURI(container = "/messages", id = id))
    val futureGraph = ResourceService.getResource(NodeService.genResourceURI(container = "/messages", id = id))
    futureGraph.map{ s => 
      if (!s.isEmpty) {
        Ok(s.get).withHeaders( (CONTENT_TYPE, "text/turtle") )
      } else {
        NotFound
      }
    }
  }
  
  def deleteMessage = {
    implicit val deleteMessageReads = (
      (__ \ 'messageUri).read[String])
    
    Action(parse.json) { request =>
      request.body.validate[String].map {
        case (messageUri) => {
          ResourceService.deleteResource(messageUri)
          Ok
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
      }
    }
  }

}