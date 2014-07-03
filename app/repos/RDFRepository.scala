package repos

import org.w3.banana._


trait RDFResourceDependencies
extends RDFModule
with RDFOpsModule
with RecordBinderModule

trait RDFRepositoryDependencies
extends RDFModule
with RDFOpsModule
with TurtleWriterModule { implicit val file = "store/jena-tdb/" }