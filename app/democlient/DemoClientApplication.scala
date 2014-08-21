package democlient

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc._

import play.api.data._
import play.api.data.Forms._

import play.api.Play.current
import play.api.libs.ws._

object DemoClientApplication extends Controller {
  
  val stnForm = Form(
    single(
      "uri" -> nonEmptyText
    )
  )
  
  val opForm = Form(
    tuple(
      "opSelect" -> nonEmptyText,
      "mywebid" -> nonEmptyText,
      "method" -> nonEmptyText,
      "requestUri" -> nonEmptyText,
      "jsonData" -> text
    )
  )
  
  def demoClient = Action {
    Ok(html.client(stnForm, opForm))
  }
  
  def getSTNSpec = Action.async { implicit request =>
    stnForm.bindFromRequest.fold(
      formWithErrors =>
        future {
          BadRequest(html.client(formWithErrors, opForm))
        },
      result => {
        WS.url(result).get().map {
          response => {
            val parser = new PlatformSpecParser(response.body)
            Ok(html.client(stnForm, opForm, Some(parser.extractPlatformDetails), response.body))
          }
        }
      })
  }
  
  import play.api.libs.json._
  
  def runOperation = Action.async { implicit request =>
    opForm.bindFromRequest.fold(
      formWithErrors => {
        println("errors!" + formWithErrors)
        future {
          BadRequest(html.client(stnForm, formWithErrors))
        }
      },
      result => {
        val webid = result._2
        val method = result._3
        val requestUri = result._4
        val jsonData = result._5
        
        runHttpCall(webid, method, requestUri, jsonData).map {
          response =>
            Ok("Status code:" + response.status + "\n" + response.body)
        }
      }
    )
  }
  
  def runHttpCall(webid: String, method: String, uri: String, jsonData: String) = {
    val holder: WSRequestHolder = WS.url(uri)
    
    val futureResponse = 
      method match {
        case "GET"  => holder.get()
        case "POST" => holder.post(Json.parse(jsonData).as[JsObject] + ("mywebid" -> JsString(webid)))
      }
    
    futureResponse
  }
  
}