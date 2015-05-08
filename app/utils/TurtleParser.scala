package utils

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._

import play.api.libs.json._

import java.net.URI


object TurtleParser extends RDFModule with SparqlGraphModule with TurtleReaderModule with JenaModule {
  
  import Ops._
  import SparqlOps._
  
  def getOne(graphStr: String, where: String, 
      varName: String, bindings: Map[String, Rdf#Node] = Map()): Option[String] = {
    
    val graph = TurtleReader.read(graphStr, "") getOrElse { return None } //sys.error("Couldn't read connection spec.")
    val engine = SparqlGraph(graph)
    
    val query = STNParser.prefixes + """
        |SELECT ?""".stripMargin +
        varName + """
        |WHERE {""".stripMargin +
        where + """
        |}""".stripMargin
    
    val row = engine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList(0)
    
    if (row.contains(varName)) {
      Some(row(varName) getOrElse sys.error("") toString())
    } else {
      None
    }
  }
  
  def getListOfOne(graphStr: String, where: String, 
      varName: String, bindings: Map[String, Rdf#Node] = Map()): Option[Seq[String]] = {
    
    val graph = TurtleReader.read(graphStr, "") getOrElse { return None } //sys.error("Couldn't read connection spec.")
    val engine = SparqlGraph(graph)
    
    val query = STNParser.prefixes + """
        |SELECT ?""".stripMargin +
        varName + """
        |WHERE {""".stripMargin +
        where + """
        |}""".stripMargin
    
    val rows = engine.executeSelect(SelectQuery(query), bindings).getOrFail()
    
    val resultsJson = JsonSolutionsWriter.asString(rows, "")  getOrElse sys.error("Couldn't serialize the query results.")
    
    Some((Json.parse(resultsJson) \\ "value").flatMap(_.asOpt[String]))
  }
  
  def injectData(data: String) = {
    
     println("reading data: " + data)
     val graph = TurtleReader.read(data, "") getOrElse sys.error("Couldn't read ttl data.")
     val engine = SparqlGraph(graph)
     
     val query = STNParser.prefixes + """
        |SELECT ?uri
        |WHERE {
        |?uri rdf:type stn:UserAccount
        |}""".stripMargin
     
     val row = engine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)
     val uri = row("uri") getOrElse sys.error("") toString()
     
     println("Injecting [" + uri + "]: " + graph)
     
     import services.ResourceService
     ResourceService.repo.injectData(makeUri(uri), graph)
  }
}