package utils

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.w3.banana._
import org.w3.banana.jena._

import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._


object TwitterCredentials {
  val PLATFORM_URI = "http:/www.twitter.com/#platform"
  val CONSUMER_KEY = "LSnVNcrG3lDfYMKrbTPWzw"
  val CONSUMER_SECRET = "TB1oAwwEqgKrkGZTtSJIu3oQpq8ahLmkokzOxlW7M"
  val TOKEN = "1691508180-LnKoiF21BuhoHqj88HT8yOOA8gVTxPgtwr21dV4"
  val TOKEN_SECRET = "Hozhj7arvyvGBuKHLVqU1jBCp2V1CQMVaHfKzYGfzZo"
}

object FacebookCredentials {
  val PLATFORM_URI = "http:/www.facebook.com/#platform"
  val CONSUMER_KEY = "125590717457632"
  val CONSUMER_SECRET = "f8683b4e4799a259613887371261b4dd"
  val TOKEN = "CAAByOV4ZA5OABAAK3hnoLIqQMU9J9pYZCZB3iQV40expBa0ZBx7cjVYKF6o9qHa5oXswzRskGEJ3efHQS5U86T1zexlrKlOSxNcmysmhdEZBhjFQPCGL15cxonNsxy8lVDqfuV6Hkt8qsTO1dN43Ut4K1YVZB5mkVX1idNb9DZCFIpbNmXshgBlF0kH1IXAcFktW2zdxjHMvNyPpPEGqOxQ"
}

object WebID {
  val CLIENT_WEB_ID = "http://api.mymanufacturer.com/tvs/874...260#thing"
}


class STNClient(specUrl: String) extends RDFModule with TurtleWriterModule with JenaModule {
  
  import Ops._
  
  import models.STNPrefix
  import models.STNOpsPrefix
  import models.STNHttpPrefix

  val clientSupportedAuth = List(STNHttpPrefix[Rdf].WebID.toString())
  val clientProduces = List(STNHttpPrefix[Rdf].Turtle.toString(), STNHttpPrefix[Rdf].JSON.toString())
  val clientConsumes = List(STNHttpPrefix[Rdf].Turtle.toString(), STNHttpPrefix[Rdf].JSON.toString())

  val rdfEntity = Map(STNOpsPrefix[Rdf].CreateUserAccount.toString() -> STNPrefix[Rdf].UserAccount.toString())
  
  val platform = STNParser.getPlatform(specUrl)
  
  def getPlatform = platform map { p => p }
  
  def getOperationDescription(opClass: String) = {
    platform map {
      p => p.operations.get(opClass)
    }
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
  
  
  def addParamsInPath(uri: String, params: List[(InputParameter, String)]): String = {
    if (params.isEmpty) {
      uri
    } else {
      val head::tail = params
      println("Param location: " + head._1.in);
      if (head._1.in == STNHttpPrefix[Rdf].Path.toString() && !head._1.key.isEmpty) {
        println("Param will be replaced, where key is: " + head._1.key.get);
        addParamsInPath(uri.replaceAll(head._1.key.get, head._2), tail)
      } else {
        addParamsInPath(uri, tail)
      }
    }
  }
  
  
  def addParamsInQuery(holder: WSRequestHolder, params: List[(InputParameter, String)]): WSRequestHolder = {
    if (params.isEmpty) holder
    else {
      val head::tail = params
      if (head._1.in == STNHttpPrefix[Rdf].Query.toString() && !head._1.key.isEmpty) {
        addParamsInQuery(holder.withQueryString((head._1.key.get, head._2)), tail)
      } else {
        addParamsInQuery(holder, tail)
      }
    }
  }
  
  def getRepresentationFormat(platformConsumes: List[String]): String = {
    if (platformConsumes.isEmpty) throw new Exception()
    
    val head::tail = platformConsumes
    
    if (clientProduces.contains(head)) {
      head
    } else {
      getRepresentationFormat(tail)
    } 
  }

  def buildTurtlePayload(opClass: String, params: List[(InputParameter, String)]): String = {
    val userAccount = STNPrefix[Rdf].UserAccount.toString()  
    rdfEntity.get(opClass) match {
        case userAccount => UserAccountTemplate.toTurtle(params)
        case _ => ""
      }
  }
  
  def addParamsInBody(platformConsumes: List[String], 
      opClass: String, params: List[(InputParameter, String)]) = {
    
      val reprFormat = getRepresentationFormat(platformConsumes)
      
      val Turtle = STNHttpPrefix[Rdf].Turtle.toString()
      reprFormat match {
        case Turtle => {
          val payload = buildTurtlePayload(opClass, params)
          println("Payload: " + payload)
          payload
        }
      }
  }
  
  def addBaseUrl(baseUrl: String, relativeUri: String): String = {
    // Some operations might require the absolute URI of a given resource
    if (relativeUri.startsWith("http://")) relativeUri
    else baseUrl + relativeUri
  }
  
  
  // TODO
  def runTwitterRequest(op: Operation, params: List[(String, String)]) {}
  // TODO
  def runFacebookRequest(op: Operation, params: List[(String, String)]) {}
  
  def runWebIDRequest(op: Operation, params: List[(InputParameter, String)]): Future[WSResponse] = {
    
    platform flatMap {
      p => {
        val uri = addBaseUrl(p.baseUrl, addParamsInPath(op.request.requestUri, params))
        val holder: WSRequestHolder = 
          addParamsInQuery(WS.url(uri), params).withHeaders("X-WebID" -> WebID.CLIENT_WEB_ID)
        
        op.request.method match {
          case "GET" => {
            println("Sending GET to [" + op.cls + "]: " + holder.queryString)
            holder.get()
          }
          case "POST" => {
            platform flatMap {
              p => holder.withHeaders("Content-Type" -> "text/turtle").post(addParamsInBody(p.consumes, op.cls, params))
            }
          }
          case "PUT" => {
            platform flatMap {
              p => holder.withHeaders("Content-Type" -> "text/turtle").put(addParamsInBody(p.consumes, op.cls, params))
            }
          }
          case "DELETE" => holder.delete()
        }
      }
    }
  }
  
  def getAuthStandard(platformSupportedAuth: List[String]): Option[String] = {
    if (platformSupportedAuth.isEmpty) return None
    
    if (clientSupportedAuth.contains(platformSupportedAuth.head)) {
      return Some(platformSupportedAuth.head)
    } else getAuthStandard(platformSupportedAuth.tail)
  }

  // Run the request using the first found supported auth standard
  def runAuthRequest(platformSupportedAuth: List[String], op: Operation, 
      params: List[(InputParameter, String)]): Option[Future[WSResponse]] = {
    
    val auth = getAuthStandard(platformSupportedAuth)
    
    if (auth.isEmpty) {
      None
    } else {
      // TODO do this better
      val webid = STNHttpPrefix[Rdf].WebID.toString()
      auth.get match {
        case webid => Some(runWebIDRequest(op, params))
      }
    }
  }
  
  
  def runOperation(op: Operation, params: List[(InputParameter, String)]): Future[WSResponse] = {
//    if (op.request.requiresAuth) {
      // check applicable auth
    
    println("Running with params: " + params)
      platform flatMap {
        p => {
          p.uri match {
//            case TwitterCredentials.PLATFORM_URI => runTwitterRequest(op, params)
//            case FacebookCredentials.PLATFORM_URI => runFacebookRequest(op, params)
            case _ => runAuthRequest(p.auth, op, params).get
          }
        }
      }
//    }
  }
  
  
}