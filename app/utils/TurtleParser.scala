package utils

import org.w3.banana._
import org.w3.banana.diesel._
import org.w3.banana.jena._

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
    
    println("Query: " + query)
        
    val row = engine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList(0)
    
    if (row.contains(varName)) {
      Some(row(varName) getOrElse sys.error("") toString())
    } else {
      None
    }
  }
}