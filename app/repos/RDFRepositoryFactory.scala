package repos

import org.w3.banana._
import com.hp.hpl.jena.tdb.TDBFactory


object RDFRepositoryFactory {
  
  def makeRDFRepository = new JenaRepository
  
  trait RDFResourceBinder extends JenaBinder
}


import org.w3.banana.jena._

trait JenaBinder extends RDFResourceDependencies with JenaModule

class JenaRepository extends RDFRepository with JenaModule {
  
  def makeRDFStore(implicit file: String): RDFStore[Rdf] = {
    JenaStore(TDBFactory.createDataset(file).asDatasetGraph())
  }
}