package repos

import org.w3.banana.jena._


object RDFRepositoryFactory {
  
  trait RDFResourceBinder extends JenaBinder
}

trait JenaBinder extends RDFResourceDependencies with JenaModule