package utils

import scala.util._
import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.w3.banana._
import org.w3.banana.jena._

import play._
import play.api.Play.current
import play.api.libs.json._
import play.api.libs.ws._



case class STNResponse(statusCode: Int, headers: Map[String, String], body: String)


class STNClient(specUrl: String) extends RDFModule with JenaModule {
  
  import Ops._
  
  import models.STNPrefix
  import models.STNOpsPrefix
  import models.STNHttpPrefix

  val TwitterPlatformUri = "http://www.twitter.com/#platform"
  val FacebookPlatformUri = "http://www.facebook.com/#platform"
  val EightTracksPlatformUri = "http://www.8tracks.com/#platform"
  
  val ClientSupportedAuth = List(STNHttpPrefix[Rdf].WebID.toString())
  val ClientWebId = "http://api.mymanufacturer.com/tvs/874260#thing"
  val ClientProduces = List(STNHttpPrefix[Rdf].Turtle.toString(), STNHttpPrefix[Rdf].JSON.toString())
  val ClientConsumes = List(STNHttpPrefix[Rdf].Turtle.toString(), STNHttpPrefix[Rdf].JSON.toString())

  val OperationEntityMapping = Map(STNOpsPrefix[Rdf].CreateUserAccount.toString() -> STNPrefix[Rdf].UserAccount.toString())
  
  val platform = STNParser.getPlatform(specUrl)
  
  
  def getOperationDescription(opClass: String) = {
    platform map {
      p => p.operations.get(opClass)
    }
  }
  
  def getAuthStandard(platformSupportedAuth: List[String]): Option[String] = {
    if (platformSupportedAuth.isEmpty) return None
    
    if (ClientSupportedAuth.contains(platformSupportedAuth.head)) {
      return Some(platformSupportedAuth.head)
    } else getAuthStandard(platformSupportedAuth.tail)
  }
  
  def getRepresentationFormat(platformConsumes: List[String]): String = {
    if (platformConsumes.isEmpty) throw new Exception()
    
    val head::tail = platformConsumes
    
    if (ClientProduces.contains(head)) {
      head
    } else {
      getRepresentationFormat(tail)
    } 
  }
  
  
  def isInPath(t: (InputParameter, String)) = if (t._1.in == STNHttpPrefix[Rdf].Path.toString()) true else false
  
  def isInQuery(t: (InputParameter, String)) = if (t._1.in == STNHttpPrefix[Rdf].Query.toString()) true else false
  
  def isInBody(t: (InputParameter, String)) = if (t._1.in == STNHttpPrefix[Rdf].Body.toString()) true else false
  
  
  def addParamsInPath(uri: String, params: List[(InputParameter, String)]): String = {
    if (params.isEmpty) {
      uri
    } else {
      val head::tail = params
      if (head._1.in == STNHttpPrefix[Rdf].Path.toString() && !head._1.key.isEmpty) {
//        println("Param will be replaced, where key is: " + head._1.key.get);
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

  
  def buildTurtlePayload(opClass: String, params: List[(InputParameter, String)]): String = {
    val userAccount = STNPrefix[Rdf].UserAccount.toString()  
//    OperationEntityMapping.get(opClass) match {
    
    if (opClass == STNOpsPrefix[Rdf].CreateUserAccount.toString()) {
      UserAccountTemplate.toTurtle(params)
    } else if (opClass == STNOpsPrefix[Rdf].CreateConnectionTo.toString() || 
                opClass == STNOpsPrefix[Rdf].DeleteConnectionTo.toString()) {
      ConnectionTemplate.toTurtle(params)
    } else {
      ""
    }
    
//    URI(opClass) match {
//        case userAccount => UserAccountTemplate.toTurtle(params)
//        case _ => ""
//      }
  }
  
  // TODO: add JSON support
  def addParamsInBody(platformConsumes: List[String], 
      opClass: String, params: List[(InputParameter, String)]) = {
    
      val reprFormat = getRepresentationFormat(platformConsumes)
      
      val Turtle = STNHttpPrefix[Rdf].Turtle.toString()
      reprFormat match {
//        case Turtle => buildTurtlePayload(opClass, params.filter(isInBody))
        case Turtle => buildTurtlePayload(opClass, params)
      }
  }
  
  def addBaseUrl(baseUrl: String, relativeUri: String): String = {
    // Some operations might require the absolute URI of a given resource
    if (relativeUri.startsWith("http://")) relativeUri
    else baseUrl + relativeUri
  }
  
  
  import org.scribe.builder._
  import org.scribe.builder.api._
  import org.scribe.model._
  
  def runTwitterRequest(uri: String, op: Operation, 
      params: List[(InputParameter, String)]): STNResponse = {
    
    val CONSUMER_KEY = "LSnVNcrG3lDfYMKrbTPWzw"
    val CONSUMER_SECRET = "TB1oAwwEqgKrkGZTtSJIu3oQpq8ahLmkokzOxlW7M"
    val TOKEN = "1691508180-LnKoiF21BuhoHqj88HT8yOOA8gVTxPgtwr21dV4"
    val TOKEN_SECRET = "Hozhj7arvyvGBuKHLVqU1jBCp2V1CQMVaHfKzYGfzZo"
    
    val service = new ServiceBuilder()
                        .provider(classOf[TwitterApi])
                        .apiKey(CONSUMER_KEY)
                        .apiSecret(CONSUMER_SECRET)
                        .build()
    
    val request =
      op.request.method match {
      
        case "GET" => ((request: OAuthRequest) => {
          for (p <- params.filter(isInQuery)) {
            request.addQuerystringParameter(p._1.key.get, p._2)
          }
          
          request
        })(new OAuthRequest(Verb.GET, uri))
        
        case "POST" => ((request: OAuthRequest) => {
          for (p <- params.filter(isInBody)) {
            request.addBodyParameter(p._1.key.get, p._2)
          }
          
          request
        })(new OAuthRequest(Verb.POST, uri))
      }
    
    service.signRequest(new Token(TOKEN, TOKEN_SECRET), request)
    val response = request.send
    
    val body = if ((response.getCode == 200 || response.getCode == 201) && op.output != None) {
//      if (op.output.get.representationFormat == STNHttpPrefix[Rdf].JSONRepresentation.toString()) {
        JSON2RDF.toRDF(response.getBody, op.output.get)
//      } else "Unknown output format"
    } else response.getBody
    
    STNResponse(response.getCode, 
        Map("Content-Type" -> response.getHeader("Content-Type")), 
        body)
  }
  
  
  def runFacebookRequest(uri: String, op: Operation,  params: List[(InputParameter, String)]): STNResponse = {
    val CONSUMER_KEY = "125590717457632"
    val CONSUMER_SECRET = "f8683b4e4799a259613887371261b4dd"
    val TOKEN = "CAAByOV4ZA5OABAIQtBXtXCBCBHEeUK8XZBYCOssGBLVQ2jRMp6P94078UYStzvpsuj0nxTb8LL7P9ijOmpkRl3mSeXDC1eAwDjqRP2y1mwDFiqmh0q6dR7XgBFSGGz5tSxxUo2rXLkPF4K12uIj0SFvl3lT2W1wwqQtMt6aZBl76WYhOHEmN6xZCaCuIbXehhAIScN2iwKTV9S2ZAJ2Bx"

    val service = new ServiceBuilder()
                        .provider(classOf[FacebookApi])
                        .apiKey(CONSUMER_KEY)
                        .apiSecret(CONSUMER_SECRET)
                        .callback("http://www.example.com/oauth_callback/")
                        .build()
    
    val request =
      op.request.method match {
      
        case "GET" => ((request: OAuthRequest) => {
          for (p <- params.filter(isInQuery)) {
            request.addQuerystringParameter(p._1.key.get, p._2)
          }
          
          request
        })(new OAuthRequest(Verb.GET, uri))
        
        case "POST" => ((request: OAuthRequest) => {
          for (p <- params.filter(isInBody)) {
            request.addBodyParameter(p._1.key.get, p._2)
          }
          
          request
        })(new OAuthRequest(Verb.POST, uri))
      }
                        
    service.signRequest(new Token(TOKEN, ""), request)
    val response = request.send

    val body = if ((response.getCode == 200 || response.getCode == 201) && op.output != None) {
      JSON2RDF.toRDF(response.getBody, op.output.get)
    } else response.getBody
    
    STNResponse(response.getCode, 
        Map("Content-Type" -> response.getHeader("Content-Type")), 
        body
      )
  }
  
  def run8TracksRequest(uri: String, op: Operation,  params: List[(InputParameter, String)]): Future[STNResponse] = {
    platform flatMap {
      p => {
        val holder: WSRequestHolder = 
          addParamsInQuery(WS.url(uri), params)
            .withHeaders("X-Api-Key" -> "d9333b84c33ebdf15dde002bda26afa409386357",
                "X-Api-Version" -> "3",
                "X-User-Token" -> "13284802;3f446f334286511673d65a129b0264b08a5e25d1")
        
        val fResp = op.request.method match {
          case "GET" => {
            holder.get()
          }
//          case "POST" => {
//            platform flatMap {
//              p => holder.withHeaders("Content-Type" -> "text/turtle").post(addParamsInBody(p.consumes, op.cls, params))
//            }
//          }
//          case "PUT" => {
//            platform flatMap {
//              p => holder.withHeaders("Content-Type" -> "text/turtle").put(addParamsInBody(p.consumes, op.cls, params))
//            }
//          }
//          case "DELETE" => holder.delete()
        }
        
        fResp.map {
          response => {
            val body = 
              if ((response.status == 200 || response.status == 201) && op.output != None) {
                JSON2RDF.toRDF(response.body, op.output.get)
              } else response.body
            
            STNResponse(response.status, 
              Map("Content-Type" -> response.header("Content-Type").getOrElse("")), 
              body)
//              response.body)
          }
        }
      }
    }
  }
  
  
  def runWebIDRequest(uri: String, op: Operation, 
      params: List[(InputParameter, String)]): Future[WSResponse] = {
    
    platform flatMap {
      p => {
        val holder: WSRequestHolder = 
          addParamsInQuery(WS.url(uri), params).withHeaders("X-WebID" -> ClientWebId)
        
        op.request.method match {
          case "GET" => {
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
  

  def addAuth(holder: WSRequestHolder, platformSupportedAuth: List[String]) = {
    val auth = getAuthStandard(platformSupportedAuth)
    
    if (auth.isEmpty) {
      holder
    } else {
      val webid = STNHttpPrefix[Rdf].WebID.toString()
      val basic = STNHttpPrefix[Rdf].BasicAuth.toString() // TODO
      auth.get match {
        case webid => {
          holder.withHeaders("X-WebID" -> ClientWebId)
        }
        case _ => holder
      }
    }
  }
  
  
  // Run the request using the first found supported auth standard
  def runRequest(uri: String, op: Operation, 
      params: List[(InputParameter, String)]): Option[Future[STNResponse]] = {
    
    
    val response = platform flatMap {
      p => {
        val holder: WSRequestHolder = addAuth(addParamsInQuery(WS.url(uri), params), p.auth)
        
        val result = op.request.method match {
          case "GET" => {
            println("running GET for URI: " + uri)
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
        
        result map {
          response => {
            val body = if ((p.cls == STNPrefix[Rdf].Platform.toString()) &&
                            (response.status == 200 || response.status == 201) && 
                            op.output != None) {
                          println("converting to RDF: " + response.body)
                          JSON2RDF.toRDF(response.body, op.output.get)
                        } else response.body
            STNResponse(response.status, 
                Map("Content-Type" -> response.header("Content-Type").getOrElse("")), 
                body)
          }
        }
      }
    }
    
    Some(response)
    
/*    val auth = getAuthStandard(platformSupportedAuth)
    
    if (auth.isEmpty) {
      None
    } else {
      // TODO do this better
      val webid = STNHttpPrefix[Rdf].WebID.toString()
      auth.get match {
        case webid => {
          val res = runWebIDRequest(uri, op, params) map {
            response => 
              STNResponse(response.status, 
                  Map("Content-Type" -> response.header("Content-Type").getOrElse("")), 
                  response.body)
          }
          Some(res)
        }
      }
    }*/
  }
  
  
  def executeOperation(op: Operation, params: List[(InputParameter, String)]): Future[STNResponse] = {
//    if (op.request.requiresAuth) {
      // check applicable auth
    
//    println("Running with params: " + params)
      platform flatMap {
        p => {
          println("params:" + params)
          println("path params:" + params.filter(isInPath))
          val requestUri = addBaseUrl(p.baseUrl, 
              addParamsInPath(op.request.requestUri, params.filter(isInPath))
            )
            
          println("URI: " + requestUri)
          
          if (p.cls == STNPrefix[Rdf].STNPlatform.toString()) {
//            runAuthRequest(requestUri, p.auth, op, params).get
            runRequest(requestUri, op, params).get
          } else {
            println("Dweet desc: " + p)
            p.uri match {
              case TwitterPlatformUri => Future(runTwitterRequest(requestUri, op, params))
              case FacebookPlatformUri => Future(runFacebookRequest(requestUri, op, params))
              case EightTracksPlatformUri => run8TracksRequest(requestUri, op, params)
//              case _ => runAuthRequest(requestUri, p.auth, op, params).get
              case _ => runRequest(requestUri, op, params).get
            }
          }
        }
      }
  }
  
  
  def itCollectParams(list: List[InputParameter], params: Map[String, String], 
      collected: List[(InputParameter, String)]) : List[(InputParameter, String)] = {
    
    if (list.isEmpty) return collected
    
    val head::tail = list
    
    val value = params.get(head.cls)
    if (!value.isEmpty) {
      itCollectParams(tail, params, collected :+ (head, value.get))
    } else {
      // TODO if required param, request user input
      itCollectParams(tail, params, collected)
    }
  }
  
  def collectParams(op: Operation, params: Map[String, String]) = {
    itCollectParams(op.request.requiredInput ++ op.request.input, params, List())
  }
  
  def isOperationSupported(opClass: String): Future[Boolean] = {
    getOperationDescription(opClass) map {
      op => op.isEmpty
    }
  }
  
  def runOperation(opClass: String, params: Map[String, String]): Future[String] = {
    getOperationDescription(opClass) flatMap {
      op => {
        if (!op.isEmpty) {
          val collectedParams = collectParams(op.get, params)
          executeOperation(op.get, collectedParams) map {
            response => response.body
          }
        } else {
          Future { "Operation not supported: " + opClass }
        }
      }
    }
  }
  
  
  def fetchURL(uri: String): Future[WSResponse] = {
    WS.url(uri).withHeaders("X-WebID" -> ClientWebId).get()
  }
  
  
}