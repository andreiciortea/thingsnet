package repos

import org.w3.banana._
import com.hp.hpl.jena.tdb.TDB
import com.hp.hpl.jena.tdb.TDBFactory


object RDFRepositoryFactory {

  trait RDFResourceDependencies extends JenaResourceDependencies
  
  def makeRDFRepository = new JenaRepository
}


import org.w3.banana.jena._

trait JenaResourceDependencies extends RDFDependencies with JenaModule

class JenaRepository extends RDFRepository with JenaModule {
  
  def makeRDFStore(implicit file: String): RDFStore[Rdf] = {
    TDB.getContext().set(TDB.symUnionDefaultGraph, true)
    JenaStore(TDBFactory.createDataset(file).asDatasetGraph())
  }
}
