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
      varName: String, isNode: Boolean, bindings: Map[String, Rdf#Node] = Map()): Option[Seq[String]] = {
    
    val graph = TurtleReader.read(graphStr, "") getOrElse { return None } //sys.error("Couldn't read connection spec.")
    val engine = SparqlGraph(graph)
    
    val query = STNParser.prefixes + """
        |SELECT ?""".stripMargin +
        varName + """
        |WHERE {""".stripMargin +
        where + """
        |}""".stripMargin
    
    val rows = engine.executeSelect(SelectQuery(query), bindings).getOrFail()
    
    Some(rows.toIterable.toList map {
      row =>
        if (isNode) {
          row(varName) getOrElse sys.error("") toString()
        } else {
          row(varName).flatMap(_.as[String]) getOrElse sys.error("") toString()
        }
    })
    
//    val resultsJson = JsonSolutionsWriter.asString(rows, "")  getOrElse sys.error("Couldn't serialize the query results.")
    
//    Some((Json.parse(resultsJson) \\ "value").flatMap(_.asOpt[String]))
  }
  
  def getListOfMany(graphStr: String, where: String, 
      vars: List[(String, Boolean)], bindings: Map[String, Rdf#Node] = Map()): List[List[Option[String]]] = {
    
    println("GRAPH: " + graphStr)
    
    val graph = TurtleReader.read(graphStr, "") getOrElse sys.error("Couldn't read connection spec.")
    val engine = SparqlGraph(graph)
    
    def buildQuery(names: List[(String, Boolean)]): String = {
      names map {
        n: (String, Boolean) => "?" + n._1 + " "
      } reduceLeft(_ + _)
    }
    
    val query = STNParser.prefixes + """
        |SELECT """.stripMargin +
        buildQuery(vars) + """
        |WHERE {""".stripMargin +
        where + """
        |}""".stripMargin
    
        
    println("QUERY: " + query)
    
    val rows = engine.executeSelect(SelectQuery(query), bindings).getOrFail().toIterable.toList
    
    println("size: " + rows.size)
    
    rows map {
      row =>
        vars map {
          v =>
            println("test: " + v._1)
            if (row.contains(v._1)) {
              println("contains!")
              if (v._2) {
                Some(row(v._1) getOrElse "" toString())
              } else {
                Some(row(v._1).flatMap(_.as[String]) getOrElse "" toString())
              }
            } else None
        }
    }
    
//    val resultsJson = JsonSolutionsWriter.asString(rows, "")  getOrElse sys.error("Couldn't serialize the query results.")
    
//    Some((Json.parse(resultsJson) \\ "value").flatMap(_.asOpt[String]))
  }
  
  def injectData(data: String, host: String) = {
    
     println("reading data: " + data)
     val graph = TurtleReader.read(data, "") getOrElse sys.error("Couldn't read ttl data.")
     val engine = SparqlGraph(graph)
     
     val query = STNParser.prefixes + """
        |SELECT ?uri
        |WHERE {
        |?uri rdf:type stn:UserAccount ;
        |      stn:hostedBy <http://""".stripMargin + host + "/assets/stnspecs/thingsnet.ttl#platform> }"
     
     val row = engine.executeSelect(SelectQuery(query)).getOrFail().toIterable.toList(0)
     val uri = row("uri") getOrElse sys.error("") toString()
     
     println("Injecting [" + uri + "]: " + graph)
     
     import services.ResourceService
     ResourceService.repo.injectData(makeUri(uri), graph)
  }
}