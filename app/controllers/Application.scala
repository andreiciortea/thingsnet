package controllers

import scala.util._

import play.api._
import play.api.mvc._

import play.api.libs.json._
import play.api.libs.functional.syntax.functionalCanBuildApplicative
import play.api.libs.functional.syntax.toFunctionalBuilderOps

import models.{Person, PersonBinder}
import models.{UserAccount, UserAccountBinder}

import org.w3.banana.jena._


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
        val account = UserAccount(Person(holderUri), displayedName, description)

        // TODO: store user account
        
        Ok("Ok")
      }
    }.recoverTotal {
      e => BadRequest("Detected error:" + JsError.toFlatJson(e))
    }
  }

}