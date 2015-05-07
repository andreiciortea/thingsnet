package repos

import services.NodeService

import org.w3.banana._

import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global


trait RDFDependencies
extends RDFModule
with RDFOpsModule
with TurtleWriterModule

trait RDFRepositoryDependencies
extends RDFDependencies
with SparqlOpsModule
with TurtleReaderModule
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
    
    op onSuccess{ case _ => println("Successfully stored triples in store.") }
  }
  
  def getRDFResource(uri: String): Future[Option[String]] = {
    val store = makeRDFStore
    val fGraph = store.getGraph(makeUri(uri))
    
    fGraph map { g =>
      if (g.isIsomorphicWith(Graph.empty)) {
        None
      } else {
        Some(TurtleWriter.asString(g, "") getOrElse sys.error("Couldn't serialize the graph."))
      }
    }
  }
  

  def fetchGraphsInList(store: RDFStore[Rdf], 
      graph: Rdf#Graph, uriList: List[String]): Future[Rdf#Graph] = {
    
    if (uriList.isEmpty) {
      Future { graph }
    } else {
      val head::tail = uriList
      
      store.getGraph(URI(head)) flatMap {
        g => fetchGraphsInList(store, graph union g, tail)
      }
    }
  }
  
  def getGraphsInList(uriList: List[String]): Future[Option[String]] = {
    fetchGraphsInList(makeRDFStore, Graph(), uriList) map {
      graph =>
        if (graph.isIsomorphicWith(Graph.empty)) {
          None
        } else {
          Some(TurtleWriter.asString(graph, "") getOrElse sys.error("Couldn't serialize the graph."))
        }
    }
  }
  
  
  def patchRDFResource(uri: String, delete: Rdf#Graph, insert: Rdf#Graph) = {
    val store = makeRDFStore
    val op = store.patchGraph(makeUri(uri), delete.toIterable, insert)
    
    op onSuccess{ case _ => println("Successfully patched graph.") }
  }
  
  def deleteRDFResource(uri: String) = {
    val store = makeRDFStore
    store.removeGraph(makeUri(uri))
  }
  
  
  import SparqlOps._
  
  def runAskQuery(query: (String, Map[String, Rdf#URI])): Boolean = {
    makeSparqlEngine.executeAsk(AskQuery(query._1), query._2) getOrFail()
  }
  
  def runSelectQuery(query: (String, Map[String, Rdf#URI])): Future[String] = {
    
    val jsonString = makeSparqlEngine.executeSelect(SelectQuery(query._1), query._2).map { results =>
      JsonSolutionsWriter.asString(results, "")  getOrElse sys.error("Couldn't serialize the query results.")
    }
    
    jsonString
  }
  
  def runConstructQuery(query: (String, Map[String, Rdf#URI])): Future[String] = {
    
    val turtleString = makeSparqlEngine.executeConstruct(ConstructQuery(query._1), query._2).map { graph =>
      TurtleWriter.asString(graph, "")  getOrElse sys.error("Couldn't serialize the query result graph.")
    }
    
    turtleString
  }
  
  def getSTNSpec = {
    val from = new java.net.URL(NodeService.getAsset("spec.ttl")).openStream()
    val graph: Rdf#Graph = TurtleReader.read(from, base = "") getOrElse sys.error("Couldn't read STN spec")
    
    TurtleWriter.asString(graph, "") getOrElse sys.error("Couldn't serialize the STN spec.")
  }
}
