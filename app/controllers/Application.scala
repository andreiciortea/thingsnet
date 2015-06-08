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
import utils.STNClient

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
  
  
  case class OpSpec(stnSpecUrl: String, opClass: String, params: Map[String, String])
  
  object OpSpec {
    implicit val writes: Writes[OpSpec] = Json.writes[OpSpec]
    implicit val reads: Reads[OpSpec] = Json.reads[OpSpec]
  }
  
  def runSTNOp = Action.async(parse.json) { request =>
    request.body.validate[OpSpec].map {
      case op => {
        val stn = new STNClient(op.stnSpecUrl)
        
        println("op is: " + op)
        
        stn.runOperation(op.opClass, op.params) map {
          content => Ok(content)
        }
      }
    }.recoverTotal {
      e => {
        println("bad req")
        Future(BadRequest)
      }
    }
  }
  
  def getSTNOp(stnSpecUrl: String, opClass: String) = Action.async {
    val stn = new STNClient(stnSpecUrl)
    
    stn.getOperationDescription(opClass) map {
      op => 
        if (op.isEmpty) NotFound
        else {
          val reqParamClasses = op.get.request.requiredInput map {
            param => param.cls
          }
          
          Ok(Json.toJson(reqParamClasses))
        }
    }
  }
  
  
  def socialTV = Action.async {
    import socialtv.SocialTV

//    SocialTV.run map {
//      output => Ok(output)
//    }
    
    SocialTV.run map {
      response => Ok(response)
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
          UserAccountController.getUserAccountUriForAgent(mywebid).map {
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
          UserAccountController.getUserAccountUriForAgent(mywebid).flatMap {
            myAccountUri =>
              ResourceService.constructGraphs(Message.queryMessagesForUser(myAccountUri.get)) map {
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