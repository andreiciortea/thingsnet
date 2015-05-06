package models

import org.w3.banana._

object STNPrefix {
  def apply[Rdf <: RDF](implicit ops: RDFOps[Rdf]) = new STNPrefix(ops)
}

class STNPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("stn", "http://purl.org/stn/core#")(ops) {
  
  val Agent = apply("Agent")
  val Person = apply("Person")
  val SmartThing = apply("SocialThing")
  
  val UserAccount = apply("UserAccount")
  val Message = apply("Message")
  
  val STNPlatform = apply("STNPlatform")
   
  val heldBy = apply("heldBy")
  val ownedBy = apply("ownedBy")
  val hostedBy = apply("hostedBy")
  val connectedTo = apply("connectedTo")
  val hasSender = apply("hasSender")
  val hasReceiver = apply("hasReceiver")
  val replyTo = apply("replyTo")
   
  val name = apply("name")
  val description = apply("description")
}


object STNOpsPrefix {
  def apply[Rdf <: RDF](implicit ops: RDFOps[Rdf]) = new STNOpsPrefix(ops)
}

class STNOpsPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("stn-ops", "http://purl.org/stn/operations#")(ops) {

  // Operations
  val CreateUserAccount = apply("CreateUserAccount")
  
  // Parameters
  val AgentURI = apply("AgentURI")
  val DisplayedName = apply("DisplayedName")
  val SocialThingOwner = apply("SocialThingOwner")
  val SocialThingClass = apply("SocialThingClass")
  
  // Properties
  val supports = apply("supports")
}


object STNHttpPrefix {
  def apply[Rdf <: RDF](implicit ops: RDFOps[Rdf]) = new STNHttpPrefix(ops)
}

class STNHttpPrefix[Rdf <: RDF](ops: RDFOps[Rdf]) extends PrefixBuilder("stn-http", "http://purl.org/stn/http#")(ops) {

  // Requests
  val STNRequest = apply("STNRequest")
  val AuthSTNRequest = apply("AuthSTNRequest")
  
  // Representations
  val Turtle = apply("Turtle")
  val TurtleRepresentation = apply("TurtleRepresentation")
  val JSON = apply("JSON")
  val JSONRepresentation = apply("JSONRepresentation")
  
  // Auth standards
  val OAuth = apply("OAuth")
  val WebID = apply("WebID")
  
  // Properties
  val supportsAuth = apply("supportsAuth")
  val consumes = apply("consumes")
  val produces = apply("produces")
}
