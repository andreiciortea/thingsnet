package models

import repos.RDFRepositoryFactory.RDFResourceBinder
import services.NodeService

import java.net.URI


case class Message(sender: URI, recevier: URI, replyTo: Option[URI], subject: Option[String], body: Option[String]) extends Resource {
  
  def getContainer = "/messages"
  
  def getURI: String = 
    NodeService.genResourceURI(container = getContainer)
}

object MessageBinder extends RDFResourceBinder {
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
}
