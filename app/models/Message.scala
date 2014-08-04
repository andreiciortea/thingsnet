package models

import repos.RDFRepositoryFactory.RDFResourceDependencies
import services.NodeService

import java.net.URI


case class Message(sender: URI, recevier: URI, replyTo: Option[URI], subject: Option[String], body: Option[String]) extends Resource {
  
  def getContainer = "/messages"
  
  def getURI: String = 
    NodeService.genResourceURI(container = getContainer, id = uuid)
  
  def toGraph = Message.messageBinder.toPG(this)
}

object Message extends RDFResourceDependencies {
  
  import Ops._
  import RecordBinder._
  
  val stn = STNPrefix[Rdf]
  
  val clazz = stn.Message
  implicit val classUris = classUrisFor[Message](clazz)

  implicit val uriBinder = new TNURIBinder[Rdf]
  val sender = property[URI](stn.hasSender)
  val receiver = property[URI](stn.hasReceiver)
  val replyTo = property[Option[URI]](stn.replyTo)
  
  val subject = property[Option[String]](stn.name)
  val body = property[Option[String]](stn.description)
  
  implicit val messageBinder = 
    pgbWithId[Message](t => Ops.URI(t.getURI))
      .apply(sender, receiver, replyTo, subject, body)(Message.apply, Message.unapply) withClasses classUris
  
  
  def queryMessagesForUser(receiverUri: String) = {
    val query = """
                |prefix : <http://purl.org/stn/core#>
                |prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                |
                |CONSTRUCT {
                |  ?messageUri rdf:type :Message .
                |  ?messageUri :hasSender ?senderUri .
                |  ?messageUri :hasReceiver ?receiverUri .
                |  ?messageUri :replyTo ?parentUri .
                |  ?messageUri :name ?subject .
                |  ?messageUri :description ?body .
                |}
                |WHERE {
                |  ?messageUri rdf:type :Message .
                |  ?messageUri :hasSender ?senderUri .
                |  ?messageUri :hasReceiver ?receiverUri .
                |  OPTIONAL { ?messageUri :replyTo [ rdf:first ?parentUri ] . }
                |  OPTIONAL { ?messageUri :name [ rdf:first ?subject ] . }
                |  OPTIONAL { ?messageUri :description [ rdf:first ?body ] . }
                |}""".stripMargin

    val bindings = Map(
        "receiverUri" -> Ops.URI(receiverUri)
      )
    
    (query, bindings)
  }
}
