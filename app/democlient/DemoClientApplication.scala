package democlient

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import play.api.mvc._

import play.api.data._
import play.api.data.Forms._

import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._

import org.scribe.builder._
import org.scribe.builder.api._
import org.scribe.model._


object DemoClientApplication extends Controller {
  
  def runTwitterRequest(method: String, uri: String, jsonData: String): (Int, String) = {
    val CONSUMER_KEY = "CONSUMER_KEY"
    val CONSUMER_SECRET = "CONSUMER_SECRET"
    val TOKEN = "TOKEN"
    val TOKEN_SECRET = "TOKEN_SECRET"

    val service = new ServiceBuilder()
                        .provider(classOf[TwitterApi])
                        .apiKey(CONSUMER_KEY)
                        .apiSecret(CONSUMER_SECRET)
                        .build()
      
    val jObjData = Json.parse(jsonData).as[JsObject]
    
    def cookPostRequest(request: OAuthRequest) = {
      for (k <- jObjData.keys) {
        request.addBodyParameter(k, (jObjData \ k).as[String])
      }
      
      request
    }
    
    val request =
        method match {
      
          case "GET" => ((request: OAuthRequest) => {
            for (k <- jObjData.keys) {
              request.addQuerystringParameter(k, (jObjData \ k).as[String])
            }
            
            request
          })(new OAuthRequest(Verb.GET, uri))
          
          case "POST" => ((request: OAuthRequest) => {
            for (k <- jObjData.keys) {
              request.addBodyParameter(k, (jObjData \ k).as[String])
            }
            
            request
          })(new OAuthRequest(Verb.POST, uri))
        }
    
    service.signRequest(new Token(TOKEN, TOKEN_SECRET), request)
    val response = request.send

    (response.getCode, response.getBody)
  }

  
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
        
        if (requestUri.startsWith("https://api.twitter.com")) {
          (
              (
                  response: (Int, String)) =>
                    future{ Ok("Status code:" + response._1 + "\n" + response._2) }
          )(runTwitterRequest(method, requestUri, jsonData))
        } else {
          runHttpRequest(webid, method, requestUri, jsonData).map {
            response =>
              Ok("Status code:" + response.status + "\n" + response.body)
          }
        }
      }
    )
  }
  
  def runHttpRequest(webid: String, method: String, uri: String, jsonData: String) = {
    val holder: WSRequestHolder = WS.url(uri)
    
    val futureResponse = 
      method match {
        case "GET"  => holder.get()
        case "POST" => holder.post(Json.parse(jsonData).as[JsObject] + ("mywebid" -> JsString(webid)))
      }
    
    futureResponse
  }
  
}