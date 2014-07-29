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
import models.{UserAccount, UserAccountBinder}
import models.Message
import services.{NodeService, ResourceService}

import java.net.URI


object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
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
          val account = UserAccount(Person(holderUri), displayedName, description, Set(new URI("http://www.johndoe.ro#me"), new URI("http://www.janedoe.ro/laptop#thing")))

          ResourceService.createResource(account)
        
          Ok("Ok")
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e))
      }
    }
  }
  
  def getUserAccount(id: String) = Action.async {
    val futureGraph = ResourceService.getResource(NodeService.genResourceURI(container = "/users", id = id))
    futureGraph.map{ s => Ok(s)}
  }
  
  def deleteUserAccount = {
    implicit val deleteUserAccountReads = (
      (__ \ 'accountUri).read[String])
    
    Action(parse.json) { request =>
      request.body.validate[String].map {
        case (accountUri) => {
          ResourceService.deleteResource(accountUri)
        
          Ok("Ok")
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e))
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
        
          ResourceService.patchResource(fromUri, UserAccountBinder.addConnection(fromUri, toUri))
        
          Ok("Ok")
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e))
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
        
          ResourceService.patchResource(fromUri, UserAccountBinder.removeConnection(fromUri, toUri))
        
          Ok("Ok")
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e))
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
          
          ResourceService.createResource(message)
        
          Ok("Ok")
        }
      }.recoverTotal {
        e => BadRequest("Detected error:" + JsError.toFlatJson(e))
      }
    }
  }

  def getMessages = TODO
  
  def getMessageById(id: String) = Action.async {
    println("id = " + id)
    println(NodeService.genResourceURI(container = "/messages", id = id))
    val futureGraph = ResourceService.getResource(NodeService.genResourceURI(container = "/messages", id = id))
    futureGraph.map{ s => Ok(s)}
  }
  
  def deleteMessage = TODO

}