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
  val Message = apply("Message")
  
  val STNPlatform = apply("STNPlatform")
   
  val heldBy = apply("heldBy")
  val hostedBy = apply("hostedBy")
  val connectedTo = apply("connectedTo")
  val hasSender = apply("hasSender")
  val hasReceiver = apply("hasReceiver")
  val replyTo = apply("replyTo")
   
  val name = apply("name")
  val description = apply("description")
}
