package controllers

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.functional.syntax.functionalCanBuildApplicative
import play.api.libs.functional.syntax.toFunctionalBuilderOps

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
  
  
  /**
   *   User Account handlers.
   */
  
  def createUserAccount = {
    implicit val createUserAccountReads = (
      (__ \ 'holderUri).read[String] and
      (__ \ 'displayedName).read[String] and
      (__ \ 'description).readNullable[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String, Option[String])].map {
        case (holderUri, displayedName, description) => {
          
          val account = UserAccount(Person(holderUri), displayedName, description)
          
          if (!ResourceService.ask(UserAccount.queryHolderExists(holderUri))) {
            ResourceService.createResource(account)
            Created(account.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
          } else {
            BadRequest("There already exists an account held by " + holderUri + ".\n")
          }
          
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
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
  
  def deleteUserAccount = {
    implicit val deleteUserAccountReads = (
      (__ \ 'accountUri).read[String])
    
    Action(parse.json) { request =>
      request.body.validate[String].map {
        case (accountUri) => {
          if (ResourceService.ask(UserAccount.queryAccountExists(accountUri))) {
            ResourceService.deleteResource(accountUri)
            Ok
          } else {
            NotFound
          }
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
      }
    }
  }
  
  
  /**
   *   Connection handlers.
   */
  
  def createConnection = {
    implicit val connectionReads = (
      (__ \ 'fromUri).read[String] and
      (__ \ 'toUri).read[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String)].map {
        case (fromUri, toUri) => {
          if (ResourceService.ask(UserAccount.queryAccountExists(fromUri)) && 
              ResourceService.ask(UserAccount.queryAccountExists(toUri))) {
            ResourceService.patchResource(fromUri, UserAccount.addConnection(fromUri, toUri))
            Created
          } else {
            BadRequest("Either the source or the target of the connection is not a registered account.\n")
          }
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
      }
    }
  }
  
  def deleteConnection = {
    implicit val connectionReads = (
      (__ \ 'fromUri).read[String] and
      (__ \ 'toUri).read[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String)].map {
        case (fromUri, toUri) => {
          if (ResourceService.ask(UserAccount.queryConnectionExists(fromUri, toUri))) {
            ResourceService.patchResource(fromUri, UserAccount.removeConnection(fromUri, toUri))
            Ok
          } else {
            NotFound
          }
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
      }
    }
  }
  
  /**
   *   Message handlers.
   */
  
  def createMessage = {
    implicit val createMessageReads = (
      (__ \ 'sender).read[String] and
      (__ \ 'receiver).read[String] and
      (__ \ 'replyTo).readNullable[String] and
      (__ \ 'subject).readNullable[String] and
      (__ \ 'body).readNullable[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String, Option[String], Option[String], Option[String])].map {
        case (sender, receiver, replyTo, subject, body) => {
          
          val optReplyTo: Option[URI] = if (replyTo.isEmpty) None else Some(new URI(replyTo.get))
          
          val message = Message(new URI(sender), new URI(receiver), optReplyTo, subject, body)
          
          if (ResourceService.ask(UserAccount.queryAccountExists(sender)) && 
              ResourceService.ask(UserAccount.queryAccountExists(receiver))) {
            ResourceService.createResource(message)
            Created(message.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
          } else {
            BadRequest("Either the sender or the receiver of the message is not a registered account.\n")
          }
          
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e) + ".\n")
      }
    }
  }

  def getMessages = {
    implicit val getMessagesReads = (
      (__ \ 'accountUri).read[String])
    
    Action.async(parse.json) { request =>
      request.body.validate[String].map {
        case (accountUri) => {
          val resultString = ResourceService.queryForGraphs(Message.queryMessagesForUser(accountUri))
          resultString.map{ s => 
            Ok(s).withHeaders( (CONTENT_TYPE, "text/turtle") )
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
    println("id = " + id)
    println(NodeService.genResourceURI(container = "/messages", id = id))
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