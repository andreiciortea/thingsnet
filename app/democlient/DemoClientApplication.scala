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
  
  def runTwitterRequest(method: String, uri: String, jsonData: String): (Int, String, String) = {
    val CONSUMER_KEY = "LSnVNcrG3lDfYMKrbTPWzw"
    val CONSUMER_SECRET = "TB1oAwwEqgKrkGZTtSJIu3oQpq8ahLmkokzOxlW7M"
    val TOKEN = "1691508180-LnKoiF21BuhoHqj88HT8yOOA8gVTxPgtwr21dV4"
    val TOKEN_SECRET = "Hozhj7arvyvGBuKHLVqU1jBCp2V1CQMVaHfKzYGfzZo"

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

    (response.getCode, response.getHeader("Content-Type"), response.getBody)
  }
  
  def runFacebookRequest(method: String, uri: String, jsonData: String): (Int, String, String) = {
    val CONSUMER_KEY = "125590717457632"
    val CONSUMER_SECRET = "f8683b4e4799a259613887371261b4dd"
    val TOKEN = "CAAByOV4ZA5OABAKqkZCPpsYMAK16CYEiz875RxfzB7qUdpgcozcyVlm3j5dtmLLsMCZC4p4zthOR9n0yYcNQv8X2rnDyA7ZCNFID9joURE8MBbY8oBfgz9EGzewZCbyi1Lp2fAVva1u0Ta0ZAhpbO71TMNqKtMHwjpKZABfBm9qaOaDspCr7M7Y6tEPbw5gatRd9GYrZAhgKJRNX3lsOFPxZC"
//    val TOKEN_SECRET = "Hozhj7arvyvGBuKHLVqU1jBCp2V1CQMVaHfKzYGfzZo"

    val service = new ServiceBuilder()
                        .provider(classOf[FacebookApi])
                        .apiKey(CONSUMER_KEY)
                        .apiSecret(CONSUMER_SECRET)
                        .callback("http://www.example.com/oauth_callback/")
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
    
    service.signRequest(new Token(TOKEN, ""), request)
    val response = request.send

    (response.getCode, response.getHeader("Content-Type"), response.getBody)
  }
  
  def runFoursquareRequest(method: String, uri: String, jsonData: String): (Int, String, String) = {
    val API_KEY = "1GYOMDKO3P00AXJVQW4PEU04FU1L3TEHE40AMTPDKQKN0ZVB"
    val API_SECRET = "ZC5AR3YLYHKMHQFU4NFGK1NG2NRQI4RZYCSSQSFR44NEOLFR"
    val TOKEN = "YS5D2DDPNBORPO1V2XFLRT0ZMVLZQAKYRPMYPH0AKKXZ4UOH"
    val TOKEN_SECRET = ""

    val service = new ServiceBuilder()
                        .provider(classOf[FoursquareApi])
                        .apiKey(API_KEY)
                        .apiSecret(API_SECRET)
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
            
            request.addQuerystringParameter("v", "20140806");
            request.addQuerystringParameter("oauth_token", TOKEN);
            
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

    (response.getCode, response.getHeader("Content-Type"), response.getBody)
  }

  
  val stnForm = Form(
    single(
      "uri" -> nonEmptyText
    )
  )
  
  val opForm = Form(
    tuple(
      //"opSelect" -> nonEmptyText,
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
        val webid = result._1
        val method = result._2
        val requestUri = result._3
        val jsonData = result._4
        
        if (requestUri.startsWith("https://api.twitter.com")) {
          (
              (
                  response: (Int, String, String)) =>
                    future{ Ok(//"Status code:" + response._1 + "\n" 
                        //+ "Content-Type:" + response._2 + "\n"
                        response._3) }
          )(runTwitterRequest(method, requestUri, jsonData))
        } else if (requestUri.startsWith("https://graph.facebook.com")) {
          (
              (
                  response: (Int, String, String)) =>
                    future{ Ok(//"Status code:" + response._1 + "\n" 
                        //+ "Content-Type:" + response._2 + "\n"
                        response._3) }
          )(runFacebookRequest(method, requestUri, jsonData))
        } else if (requestUri.startsWith("https://api.foursquare.com")) {
          (
              (
                  response: (Int, String, String)) =>
                    future{ Ok("Status code:" + response._1 + "\n" 
                        + "Content-Type:" + response._2 + "\n"
                        + response._3) }
          )(runFoursquareRequest(method, requestUri, jsonData))
        } else {
          runHttpRequest(webid, method, requestUri, jsonData).map {
            response =>
              Ok("Status code: " + response.status + "\n"
                  + "Content-Type: " + (response.header("Content-Type") getOrElse "") + "\n"
                  + response.body)
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
        case "POST" => {
          println("posting request with webid: " + webid)
          holder.post(Json.parse(jsonData).as[JsObject] + ("mywebid" -> JsString(webid)))
        }
      }
    
    futureResponse
  }
  
}