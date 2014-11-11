package democlient

import scala.util._

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._
import org.w3.banana.syntax._
import play.api.libs.json._
import play.api.libs.functional.syntax.toFunctionalBuilderOps
import play.api.libs.functional.syntax.functionalCanBuildApplicative


case class PlatformSpec(name: String, uri: String, baseUrl: String, operations: List[OperationSpec])

object PlatformSpec {
  implicit val writes: Writes[PlatformSpec] = Json.writes[PlatformSpec]
  implicit val reads: Reads[PlatformSpec] = Json.reads[PlatformSpec]
}


case class OperationSpec(id: String, cls: String, method: String, requestUri: String, params: List[ParameterSpec], out: Option[OutputSpec])

object OperationSpec {
  implicit val writes: Writes[OperationSpec] = Json.writes[OperationSpec]
  implicit val reads: Reads[OperationSpec] = Json.reads[OperationSpec]
}


case class ParameterSpec(cls: String, name: String, required: Boolean)

object ParameterSpec {
  implicit val writes: Writes[ParameterSpec] = Json.writes[ParameterSpec]
  implicit val reads: Reads[ParameterSpec] = Json.reads[ParameterSpec]
}


case class PropMapping(jsonPath: String, prop: String)

object PropMapping {
  implicit val writes: Writes[PropMapping] = Json.writes[PropMapping]
  implicit val reads: Reads[PropMapping] = Json.reads[PropMapping]
}

case class OutputSpec(cls: String, id: String, fields: List[PropMapping])

object OutputSpec {
  implicit val writes: Writes[OutputSpec] = Json.writes[OutputSpec]
  implicit val reads: Reads[OutputSpec] = Json.reads[OutputSpec]
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
                |SELECT ?platformName ?platformUri ?baseUrl
                |WHERE {
                |  ?platformUri rdf:type :Platform .
                |  ?platformUri :name ?platformName .
                |  ?platformUri stn-http:baseURL ?baseUrl .
                |}""".stripMargin
    
    val row = sparqlEngine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)
    
    val platformName = row("platformName").flatMap(_.as[String])  getOrElse sys.error("") toString()
    val platformUri = row("platformUri")  getOrElse sys.error("") toString()
    val baseUrl = row("baseUrl")  getOrElse sys.error("") toString()
    
    PlatformSpec(platformName, platformUri, baseUrl, extractOps(platformUri))
  }
  
  def extractOps(platformUri: String): List[OperationSpec] = {
    val query = """
                |prefix : <http://purl.org/stn/operations#>
                |prefix stn-http: <http://purl.org/stn/http#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |prefix http: <http://www.w3.org/2011/http#>
                |
                |SELECT ?opUri ?opClass ?method ?requestUri
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
          getOpParams(opUri),
          getOpOut(opUri))
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
                        |  ?opUri :hasInput [
                        |      a ?paramClass ;
                        |      :required true ;
                        |      :paramName ?paramName ;
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
  
    def getOpOut(opUri: String): Option[OutputSpec] = {
      val outParamsQuery = """
                        |prefix : <http://purl.org/stn/operations#>
                        |prefix stn: <http://purl.org/stn/core#>
                        |prefix stn-http: <http://purl.org/stn/http#>
                        |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                        |prefix http: <http://www.w3.org/2011/http#>
                        |
                        |SELECT ?subjectClass ?subjectIdKey ?jsonPath ?reflectedProp
                        |WHERE {
                        |  ?opUri :hasOutput [
                        |      a stn-http:JSONRepresentation ;
                        |       :representationOf ?subjectClass ;
                        |       :contains
                        |           [ :path ?subjectIdKey ;
                        |               :reflects stn:id ;
                        |           ] ;
                        |       :contains
                        |           [ :path ?jsonPath ;
                        |               :reflects ?reflectedProp ;
                        |           ] ;
                        |  ] .
                        |}""".stripMargin
        
        val bindings = Map(
            "opUri" -> URI(opUri)
          )
        
        val rowsQ2 = sparqlEngine.executeSelect(SelectQuery(outParamsQuery), bindings).getOrFail().toIterable.toList
        
        if (rowsQ2.size > 0) {
            val subjectClass = rowsQ2(0)("subjectClass")  getOrElse sys.error("") toString()
            val subjectIdKey = rowsQ2(0)("subjectIdKey").flatMap(_.as[String])  getOrElse sys.error("") toString()
            
            val mapping = rowsQ2 map {
              row => PropMapping(row("jsonPath").flatMap(_.as[String]) getOrElse sys.error("") toString(),
                        "" + (row("reflectedProp") getOrElse sys.error(""))
                      )
            }
        
            return Some(OutputSpec(subjectClass, subjectIdKey, mapping))
        }
    
        None
    }
    
}