package democlient

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api._
import play.api.mvc._

import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._
import play.api.libs.functional.syntax._

object WWWClient extends Controller {

  def index = Action {
    Ok(html.wwwclient())
  }
  
  def getPlatformSpec(uri: String) = Action.async {
    WS.url(java.net.URLDecoder.decode(uri, "UTF-8")).get().map {
      response => {
        val parser = new PlatformSpecParser(response.body)
//        Ok(html.client(stnForm, opForm, Some(parser.extractPlatformDetails), response.body))
        Ok(Json.toJson(parser.extractPlatformDetails))
      }
    }
//    Ok("Twitter dummy spec: " + uri)
  }
}