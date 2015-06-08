package utils

import org.w3.banana._
import org.w3.banana.binder._
import org.w3.banana.diesel._
import org.w3.banana.jena._

trait Resource
extends RDFModule
with RDFOpsModule
with TurtleWriterModule
with JenaModule


// TOOD: a more versatile template engine
abstract class TurtleTemplate {

  import models.STNPrefix
  import models.STNOpsPrefix
  import models.STNHttpPrefix

  // TODO
  def prefixes = """
            |@prefix stn: <http://purl.org/stn/core#> .
            |@prefix stn-ops: <http://purl.org/stn/operations#> .
            |@prefix stn-http: <http://purl.org/stn/http#> .
            |@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            |@prefix http: <http://www.w3.org/2011/http#> .
            """.stripMargin
  
  def getTemplate: List[String]
  
  def tag(s: String) = "__" + s + "__"
  
  def addParams(template: String, params: List[(InputParameter, String)]): String = {
    if (params.isEmpty) {
      template
    } else {
      val head::tail = params
      addParams(template.replaceAll(tag(head._1.cls), head._2), tail)
    }
  }
  
  def buildTurtle(template: List[String], 
      params: List[(InputParameter, String)], result: List[String]): String = {
    
    if (template.isEmpty) {
      result.mkString("\n")
    } else {
      val head::tail = template
      val headWithParams = addParams(head, params)
      
      if (headWithParams.contains("__")) {
        buildTurtle(tail, params, result)
      } else {
        buildTurtle(tail, params, result :+ headWithParams)
      }
    }
  }
  
  def toTurtle(params: List[(InputParameter, String)]): String = {
//    prefixes + buildTurtle(getTemplate, params, List())
    buildTurtle(getTemplate, params, List("<> a <http://purl.org/stn/core#UserAccount> ."))
  }
}

object UserAccountTemplate extends TurtleTemplate {
  
  // TODO add short link feature
  def getTemplate = List("<> <http://purl.org/stn/core#name> \"__http://purl.org/stn/operations#DisplayedName__\" .",
      "<> <http://purl.org/stn/core#description> <__http://purl.org/stn/operations#Description__> .",
      "<> <http://purl.org/stn/core#heldBy> _:something .",
      "_:something a <__http://purl.org/stn/operations#SocialThingClass__> .",
      "_:something <http://purl.org/stn/core#ownedBy> <__http://purl.org/stn/operations#SocialThingOwner__> .")
}

object ConnectionTemplate extends TurtleTemplate {
  
  // TODO add short link feature
  def getTemplate = List("<> <http://purl.org/stn/core#connectedTo> <__http://purl.org/stn/operations#AgentURI__> .")
}