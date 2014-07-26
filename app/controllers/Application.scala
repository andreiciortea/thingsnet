package controllers

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.functional.syntax.functionalCanBuildApplicative
import play.api.libs.functional.syntax.toFunctionalBuilderOps

import models.Agent
import models.UserAccount
import services.{NodeService, ResourceService}


object Application extends Controller {

  def index = Action {
    Ok(views.html.index("Your new application is ready."))
  }
  
  implicit val createUserAccountReads = (
    (__ \ 'holderUri).read[String] and
    (__ \ 'displayedName).read[String] and
    (__ \ 'description).readNullable[String]) tupled
  
  def createUserAccount = Action(parse.json) { request =>
    request.body.validate[(String, String, Option[String])].map {
      case (holderUri, displayedName, description) => {
        val account = UserAccount(Agent(holderUri), displayedName, description)

        ResourceService.createResource(account)
        
        Ok("Ok")
      }
    }.recoverTotal {
      e => BadRequest("Detected error:" + JsError.toFlatJson(e))
    }
  }
  
  def getUserAccount(id: String) = Action.async {
    val futureGraph = ResourceService.getResource(NodeService.genResourceURI(container = "/users", id = id))
    futureGraph.map{ s => Ok(s)}
  }
  
  implicit val deleteUserAccountReads = (
    (__ \ 'accountUri).read[String])
  
  def deleteUserAccount = Action(parse.json) { request =>
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