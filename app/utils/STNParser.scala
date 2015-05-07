package utils

import scala.util._

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._
import org.w3.banana.syntax._
import play.api.libs.json._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.functional.syntax.functionalCanBuildApplicative

import play.api.Play.current
import play.api.libs.ws._
import scala.concurrent.ExecutionContext.Implicits.global



case class HTTPRequest(cls: String, method: String, requestUri: String, 
    requiresAuth: Boolean, requiredInput: List[InputParameter], input: List[InputParameter])


case class Platform(uri: String, cls: String, name: String, baseUrl: String, auth: List[String], 
    consumes: List[String], produces: List[String], operations: Map[String, Operation])

object Platform extends RDFModule with SparqlGraphModule with JenaModule {
  
  import Ops._
  import SparqlOps._
  
  import models.STNOpsPrefix
  import models.STNHttpPrefix
  
  // Returns a list of URIs that are object to a given predicate
  def extractObjectsByPredicate(engine: SparqlEngine[Rdf], platformUri: String, predicate: String) = {
    
    val query = STNParser.prefixes + """
            |SELECT ?object
            |WHERE {
            |  ?platformUri ?predicate ?object .
            |}""".stripMargin
    
    val bindings = Map(
        "platformUri" -> URI(platformUri),
        "predicate" -> URI(predicate)
      )
    
    val rows = engine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList
    
    rows map {
      row => row("object")  getOrElse sys.error("") toString()
    }
  }
  
  def extractOperations(engine: SparqlEngine[Rdf], platformUri: String) = {
    val opUris = extractObjectsByPredicate(engine, platformUri, STNOpsPrefix[Rdf].supports.toString())
    
    import scala.collection.TraversableOnce
    
    opUris map {
      opUri => {
        val op = Operation.extract(engine, opUri)
        (op.cls, op)
      }
    }
  }
  
  def extract(engine: SparqlEngine[Rdf]) = {
    val query = STNParser.prefixes + """
            |SELECT ?platformUri ?platformName ?baseUrl
            |WHERE {
            |  ?platformUri rdf:type stn:LDPlatform .
            |  ?platformUri stn:name ?platformName .
            |  ?platformUri stn-http:baseURL ?baseUrl .
            |}""".stripMargin
    
    val row = engine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)

    val platformUri = row("platformUri")  getOrElse sys.error("") toString()
    val platformName = row("platformName").flatMap(_.as[String])  getOrElse sys.error("") toString()
    val baseUrl = row("baseUrl")  getOrElse sys.error("") toString()

    Platform(platformUri, "stn:LDPlatform", platformName, baseUrl, 
        extractObjectsByPredicate(engine, platformUri, STNHttpPrefix[Rdf].supportsAuth.toString()),
        extractObjectsByPredicate(engine, platformUri, STNHttpPrefix[Rdf].consumes.toString()),
        extractObjectsByPredicate(engine, platformUri, STNHttpPrefix[Rdf].produces.toString()),
        extractOperations(engine, platformUri).toMap)
  }
}


case class Operation(uri: String, cls: String, request: HTTPRequest)

object Operation extends RDFModule with SparqlGraphModule with JenaModule {
  
  import Ops._
  import SparqlOps._
  
  def extract(engine: SparqlEngine[Rdf], opUri: String) = {
    val query = STNParser.prefixes + """
                |SELECT ?opClass ?requestClass ?method ?requestUri
                |WHERE {
                |  ?opUri rdf:type ?opClass .
                |  ?opUri stn-ops:implementedAs [
                |    a ?requestClass ;
                |    http:methodName ?method ;
                |    http:requestURI ?requestUri ;
                |  ] .
                |}""".stripMargin
    
    val bindings = Map(
        "opUri" -> URI(opUri)
      )
    
    val row = engine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList(0)
    
    val opClass = row("opClass") getOrElse sys.error("") toString()
    val requestClass = row("requestClass") getOrElse sys.error("") toString()
    val method = row("method").flatMap(_.as[String]) getOrElse sys.error("")
    val requestUri = row("requestUri").flatMap(_.as[String]) getOrElse sys.error("")

    import models.STNHttpPrefix
    
    val auth = if (URI(requestClass) == STNHttpPrefix[Rdf].AuthSTNRequest) true else false
    
    Operation(opUri, opClass, 
        HTTPRequest(requestClass, method, requestUri, auth, 
            InputParameter.extract(engine, opUri, true), 
            InputParameter.extract(engine, opUri, false)
        )
    )
  }
}


case class InputParameter(cls: String, in: String, isRequired: Boolean, key: Option[String])

object InputParameter extends RDFModule with SparqlGraphModule with JenaModule {
  
  import Ops._
  import SparqlOps._
  
  def extract(engine: SparqlEngine[Rdf], opUri: String, required: Boolean) = {
    /*val query = STNParser.prefixes + """
                |SELECT ?paramClass ?in
                |WHERE {
                |  ?opUri """.stripMargin +
                (if (required) "stn-ops:hasRequiredInput" else "stn-ops:hasInput") + """
                |    [ a ?paramClass ;
                |        stn-http:paramIn ?in ;
                |    ] .
                |}""".stripMargin*/

    val query = STNParser.prefixes + """
                |SELECT ?paramUri ?paramClass ?in ?paramKey
                |WHERE {
                |  ?opUri """.stripMargin +
                (if (required) "stn-ops:hasRequiredInput" else "stn-ops:hasInput") + 
                """
                |    ?paramUri .
                |  ?paramUri a ?paramClass ;
                |          stn-http:paramIn ?in .
                |  OPTIONAL { ?paramUri stn-http:paramKey ?paramKey }
                |}""".stripMargin
    
    val bindings = Map(
        "opUri" -> URI(opUri)
    )
    
    val rows = engine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList
    
    rows map {
      row => InputParameter(row("paramClass") getOrElse sys.error("") toString(),
            row("in") getOrElse sys.error("") toString(),
            required,
            if (row("paramKey").isSuccess) 
              Some(row("paramKey").flatMap(_.as[String]).get.toString()) else None
            )
    }
  }
}


object STNParser extends RDFModule with SparqlGraphModule with JenaModule {

  import Ops._
  import SparqlOps._

  def prefixes = """
              |prefix stn: <http://purl.org/stn/core#>
              |prefix stn-ops: <http://purl.org/stn/operations#>
              |prefix stn-http: <http://purl.org/stn/http#>
              |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
              |prefix http: <http://www.w3.org/2011/http#>
              """.stripMargin
  
  def fetchSTNDescDoc(specURL: String) = {
    WS.url(java.net.URLDecoder.decode(specURL, "UTF-8")).get().map {
      response => {
        response.body
      }
    } 
  }
  
  def getPlatform(specUrl: String) = {
    fetchSTNDescDoc(specUrl) map {
      spec => {
        val graph = TurtleReader.read(spec, "") getOrElse sys.error("Couldn't read the STN spec.")
        Platform.extract(SparqlGraph(graph))
      }
    }
  }
  
}