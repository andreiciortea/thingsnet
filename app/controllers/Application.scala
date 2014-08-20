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
import models.PlatformSpecParser
import services.{NodeService, ResourceService}

import java.net.URI

import play.api.data._
import play.api.data.Forms._

import play.api.Play.current
import play.api.libs.ws._


object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  def spec = Action {
    Ok(ResourceService.getSTNSpec)
  }
  
  
  val stnForm = Form(
    single(
      "uri" -> nonEmptyText
    )
  )
  
  val opForm = Form(
    tuple(
      "method" -> nonEmptyText,
      "uri" -> nonEmptyText,
      "params" -> text
    )
  )
  
  def demoClient = Action {
    Ok(views.html.client(stnForm, opForm))
  }
  
  def getSTNSpec = Action.async { implicit request =>
    stnForm.bindFromRequest.fold(
      formWithErrors =>
        future {
          BadRequest(views.html.client(formWithErrors, opForm))
        },
      result => {
        WS.url(result).get().map {
          response => {
            val parser = new PlatformSpecParser(response.body)
            Ok(views.html.client(stnForm, opForm, Some(parser.extractPlatformDetails), response.body))
          }
        }
      })
  }
  
  def runOperation = Action {
    // TODO
    Ok(views.html.index("bla"))
  }
  
  
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
          
          val account = UserAccount(Person(mywebid), displayedName, description)
          
          if (!ResourceService.ask(UserAccount.queryHolderExists(mywebid))) {
            ResourceService.createResource(account)
            Created(account.toTurtle).withHeaders( (CONTENT_TYPE, "text/turtle") )
          } else {
            BadRequest("There already exists an account held by " + mywebid + ".\n")
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
      (__ \ 'mywebid).read[String])
    
    Action(parse.json) { request =>
      request.body.validate[String].map {
        case (mywebid) => {
          if (ResourceService.ask(UserAccount.queryAccountExists(mywebid))) {
            ResourceService.deleteResource(mywebid)
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
      (__ \ 'mywebid).read[String] and
      (__ \ 'accountUri).read[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String)].map {
        case (mywebid, accountUri) => {
          if (ResourceService.ask(UserAccount.queryAccountExists(mywebid)) && 
              ResourceService.ask(UserAccount.queryAccountExists(accountUri))) {
            ResourceService.patchResource(mywebid, UserAccount.addConnection(mywebid, accountUri))
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
      (__ \ 'mywebid).read[String] and
      (__ \ 'accountUri).read[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String)].map {
        case (mywebid, accountUri) => {
          if (ResourceService.ask(UserAccount.queryConnectionExists(mywebid, accountUri))) {
            ResourceService.patchResource(mywebid, UserAccount.removeConnection(mywebid, accountUri))
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
  // TODO: body param should be required
  def createMessage = {
    implicit val createMessageReads = (
      (__ \ 'mywebid).read[String] and
      (__ \ 'receiver).read[String] and
      (__ \ 'replyTo).readNullable[String] and
      (__ \ 'subject).readNullable[String] and
      (__ \ 'body).readNullable[String]) tupled
    
    Action(parse.json) { request =>
      request.body.validate[(String, String, Option[String], Option[String], Option[String])].map {
        case (mywebid, receiver, replyTo, subject, body) => {
          
          val optReplyTo: Option[URI] = if (replyTo.isEmpty) None else Some(new URI(replyTo.get))
          
          val message = Message(new URI(mywebid), new URI(receiver), optReplyTo, subject, body)
          
          if (ResourceService.ask(UserAccount.queryAccountExists(mywebid)) && 
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
      (__ \ 'mywebid).read[String])
    
    Action.async(parse.json) { request =>
      request.body.validate[String].map {
        case (mywebid) => {
          val resultString = ResourceService.queryForGraphs(Message.queryMessagesForUser(mywebid))
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