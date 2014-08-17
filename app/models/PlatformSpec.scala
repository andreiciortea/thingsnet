package models

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._
import org.w3.banana.syntax._
import play.api.libs.json._
import play.api.libs.functional.syntax.toFunctionalBuilderOps


case class PlatformSpec(uri: String, baseUrl: String, operations: List[OperationSpec])


case class OperationSpec(id: String, cls: String, method: String, requestUri: String, params: List[ParameterSpec])

object OperationSpec {
  implicit val writes: Writes[OperationSpec] = Json.writes[OperationSpec]
}


case class ParameterSpec(cls: String, name: String, required: Boolean)

object ParameterSpec {
  implicit val writes: Writes[ParameterSpec] = Json.writes[ParameterSpec]
}


class PlatformSpecParser(ttlSpec: String) extends RDFModule with SparqlGraphModule with TurtleReaderModule with JenaModule {
  
  import Ops._
  import SparqlOps._

  val graph = TurtleReader.read(ttlSpec, "") getOrElse sys.error("Couldn't read the STN spec.");
  val sparqlEngine = SparqlGraph(graph)
  
  def extractPlatformDetails: PlatformSpec = {
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |prefix stn-http: <http://purl.org/stn/http#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |
                |SELECT ?platformUri ?baseUrl
                |WHERE {
                |  ?platformUri rdf:type :Platform .
                |  ?platformUri stn-http:baseURL ?baseUrl
                |}""".stripMargin
    
    val row = sparqlEngine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)
    
    val platformUri = row("platformUri")  getOrElse sys.error("") toString()
    val baseUrl = row("baseUrl")  getOrElse sys.error("") toString()
    
    PlatformSpec(platformUri, baseUrl, extractOps(platformUri))
  }
  
  def extractOps(platformUri: String): List[OperationSpec] = {
    val query = """
                |prefix : <http://purl.org/stn/operations#>
                |prefix stn-http: <http://purl.org/stn/http#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |prefix http: <http://www.w3.org/2011/http#>
                |
                |SELECT ?opUri ?opClass ?method ?requestUri ?requestParamDesc
                |WHERE {
                |  ?platformUri :supports ?opUri .
                |  ?opUri rdf:type ?opClass .
                |  ?opUri :implementedAs [
                |    a stn-http:STNRequest ;
                |    http:methodName ?method ;
                |    http:requestURI ?requestUri ;
                |  ] .
                |} ORDER BY ?opClass""".stripMargin
    
    val bindings = Map(
        "platformUri" -> URI(platformUri)
      )
    
    sparqlEngine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList map {
      row => {
        val opUri = row("opUri") getOrElse sys.error("") toString()
        
        OperationSpec (opUri,
          row("opClass") getOrElse sys.error("") toString(),
          row("method").flatMap(_.as[String]) getOrElse sys.error(""),
          row("requestUri").flatMap(_.as[String]) getOrElse sys.error(""),
          getOpParams(opUri))
      }
    }
  }
  
  def getOpParams(opUri: String): List[ParameterSpec] = {
    val requiredParamsQuery = """
                        |prefix : <http://purl.org/stn/operations#>
                        |prefix stn-http: <http://purl.org/stn/http#>
                        |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                        |prefix http: <http://www.w3.org/2011/http#>
                        |
                        |SELECT ?paramClass ?paramName
                        |WHERE {
                        |  ?opUri :implementedAs [
                        |    a stn-http:STNRequest ;
                        |    :requiresInput [
                        |      a ?paramClass ;
                        |      :paramName ?paramName ;
                        |    ] ;
                        |  ] .
                        |}""".stripMargin
    
    val bindings = Map(
        "opUri" -> URI(opUri)
      )
    
    val rowsQ2 = sparqlEngine.executeSelect(SelectQuery(requiredParamsQuery), bindings).getOrFail().toIterable.toList
    val params = rowsQ2 map {
      row => ParameterSpec(row("paramClass") getOrElse sys.error("") toString(),
                row("paramName").flatMap(_.as[String]) getOrElse sys.error(""),
                true
              )
    }
    
    params
    
  }
  
}