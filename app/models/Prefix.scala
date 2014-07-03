package models

import org.w3.banana._

object STNPrefix {
  def apply[Rdf <: RDF](implicit ops: RDFOps[Rdf]) = new STNPrefix(ops)
}

class STNPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("stn", "http://purl.org/stn/core#")(ops) {
  
  val Agent = apply("Agent")
  val Person = apply("Person")
  val SmartThing = apply("SmartThing")
  
  val UserAccount = apply("UserAccount")
   
  val heldBy = apply("heldBy")
   
  val name = apply("name")
  val description = apply("description")
}
