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

import java.net.URI


object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def spec = Action {
    Ok(ResourceService.getSTNSpec)
  }

  
  // TODO: review returned HTTP status codes
  
  /**
   *   User Account handlers.
   */
  
  def createUserAccount = {
    implicit val createUserAccountReads = (
      (__ \ 'mywebid).read[String] and
      (__ \ 'displayedName).read[String] and
      (__ \ 'description).readNullable[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String, Option[String])].map {
        case (mywebid, displayedName, description) => {
          
          val account = UserAccount(SmartThing(mywebid), new URI(NodeService.getPlatformURI), displayedName, description)
          
          if (!ResourceService.ask(UserAccount.queryHolderExists(mywebid))) {
            ResourceService.createResource(account)
            Created(account.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
          } else {
            Forbidden("There already exists an account held by " + mywebid + ".\n")
          }
          
        }
      }.recoverTotal {
        // TODO: refactor this for all requests
        e => {
          if (JsError.toFlatJson(e).toString().contains("mywebid")) {
            Unauthorized
          } else {
            BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
          }
        }
      }
    }
  }
  
  def getUserAccount(id: String) = Action.async {
    val futureGraphString = ResourceService.getResource(NodeService.genResourceURI(container = "/users", id = id))
    futureGraphString.map{ s =>
      if (!s.isEmpty) {
          Ok(s.get).withHeaders( (CONTENT_TYPE, "text/turtle") )
      } else {
        NotFound
      }
    }
  }
  
  def getUserAccountByWebID(userWebID: String) = Action.async {
    getAccountByWebID(userWebID).flatMap {
      accountUri => {
        ResourceService.getResource(accountUri).map{ s =>
          Ok(s.get).withHeaders( (CONTENT_TYPE, "text/turtle") )
        }
      }
    }.recover {
      case e: Exception => BadRequest(e.getMessage)
    }
  }
  
  // TODO: proper results parsing
  // Helper function, not an HTTP request handler.
  def getAccountByWebID(webId: String): Future[String] = {
      println("Checking account for " + webId)
      ResourceService.queryForResults(UserAccount.queryAccountByHolder(webId)).map {
        resultsJson => {
          val results = (Json.parse(resultsJson) \\ "value")
            if (results.isEmpty) {
              throw new Exception("No account registered for this WebID.")
          } else {
            results(0).as[String]
          }
        }
      }
  }
  
  def deleteUserAccount = {
    implicit val deleteUserAccountReads = (
      (__ \ 'mywebid).read[String])
    
    Action.async(parse.json) { request =>
      request.body.validate[String].map {
        case (mywebid) => {
          getAccountByWebID(mywebid).map {
            accountUri => {
              ResourceService.deleteResource(accountUri)
              Ok
            }
          }.recover {
            case e: Exception => BadRequest(e.getMessage)
          }
        }
      }.recoverTotal {
        e => future {
          Unauthorized
        }
      }
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
          getAccountByWebID(mywebid).flatMap {
            myAccountUri => {
              
//              if (ResourceService.ask(UserAccount.queryAccountExists(targetUri))) {
//              } else {
//                  BadRequest("The target of the connection is not a registered account.\n")
//              }
              
              getAccountByWebID(targetUri).map {
                targetAccountUri => {
                  ResourceService.patchResource(myAccountUri, UserAccount.addConnection(myAccountUri, targetAccountUri))
                  Created
                }
              }.recover {
                // TODO: do some validation before adding a connection to the provided URI.
                case e: Exception => {
                  ResourceService.patchResource(myAccountUri, UserAccount.addConnection(myAccountUri, targetUri))
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
          getAccountByWebID(mywebid).flatMap {
            myAccountUri => {
              
              // TODO: refactor this
              if (ResourceService.ask(UserAccount.queryConnectionExists(myAccountUri, targetUri))) {
                ResourceService.patchResource(myAccountUri, UserAccount.removeConnection(myAccountUri, targetUri))
                future { Ok }
              } else {
                getAccountByWebID(targetUri).map {
                  targetAccountUri => 
                    if (ResourceService.ask(UserAccount.queryConnectionExists(myAccountUri, targetAccountUri))) {
                      ResourceService.patchResource(myAccountUri, UserAccount.removeConnection(myAccountUri, targetAccountUri))
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
          getAccountByWebID(mywebid).map {
            myAccountUri => {  
              val optReplyTo: Option[URI] = if (replyTo.isEmpty) None else Some(new URI(replyTo.get))
              
              val message = Message(new URI(myAccountUri), new URI(receiver), optReplyTo, subject, body)
              
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
          getAccountByWebID(mywebid).flatMap {
            myAccountUri =>
              ResourceService.queryForGraphs(Message.queryMessagesForUser(myAccountUri)) map {
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