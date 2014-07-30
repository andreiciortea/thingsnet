package repos

import org.w3.banana._

import scala.concurrent.ExecutionContext.Implicits.global


trait RDFResourceDependencies
extends RDFModule
with RDFOpsModule
with RecordBinderModule

trait RDFRepositoryDependencies
extends RDFModule
with RDFOpsModule
with SparqlOpsModule
with TurtleWriterModule
with JsonSolutionsWriterModule { implicit val file = "store/jena-tdb/" }


abstract class RDFRepository extends RDFRepositoryDependencies {
  import Ops._
  
  def makeRDFStore(implicit file: String): RDFStore[Rdf]
  
  def makeSparqlEngine: SparqlEngine[Rdf] = SparqlEngine[Rdf](makeRDFStore)
  
  def createRDFResource(uri: String, graph: PointedGraph[Rdf]) {
    val store = makeRDFStore
    println(uri)
    println(graph.graph)
    val op = store.appendToGraph(makeUri(uri), graph.graph)
    
    op onSuccess{ case _ => println("Successfully stored triples in store") }
  }
  
  def getRDFResource(uri: String) = {
    val store = makeRDFStore
    val fGraph = store.getGraph(makeUri(uri))
    
    val turtleString = fGraph map { g =>
      TurtleWriter.asString(g, "") getOrElse sys.error("coudn't serialize the graph")
    }
    
    turtleString
  }
  
  def patchRDFResource(uri: String, delete: Rdf#Graph, insert: Rdf#Graph) = {
    val store = makeRDFStore
    val op = store.patchGraph(makeUri(uri), delete.toIterable, insert)
    
    op onSuccess{ case _ => println("Successfully patched graph") }
  }
  
  def deleteRDFResource(uri: String) = {
    val store = makeRDFStore
    store.removeGraph(makeUri(uri))
  }
  
  
  import SparqlOps._
  
  def runQuery(query: (String, Map[String, Rdf#URI])) = {
    
    val jsonString = makeSparqlEngine.executeSelect(SelectQuery(query._1), query._2).map { results =>
      JsonSolutionsWriter.asString(results, "")  getOrElse sys.error("coudn't serialize the query results")
    }
    
    jsonString
  }
}
