package controllers

import play.api.mvc._

import utils.TurtleParser
import services.ResourceService


object ScenarioControllers extends Controller {

  val basePath = "/Users/andreiciortea/Work/workspace/scala/thingsnet/public/scenario/"
  
  val files = Map("dave" -> List(basePath + "dave.ttl"),
        "jane" -> List(basePath + "jane.ttl", basePath + "janetv.ttl"),
        "mike" -> List(basePath + "mike.ttl", basePath + "miketv.ttl"),
        "bob" -> List(basePath + "bob.ttl", basePath + "bobtv.ttl"),
        "john" -> List(basePath + "john.ttl", basePath + "johntv.ttl"))
  
  
  def loadScenario(scenario: String) = Action { request =>
    for (f <- files.get(scenario).get) {
      val source = scala.io.Source.fromFile(f)
      val lines = try source.mkString finally source.close()
      
      TurtleParser.injectData(lines, request.host)
    }
    
    Ok
  }
  
  def unloadScenario(scenario: String) = Action {
    for (f <- files.get(scenario).get) {
      val source = scala.io.Source.fromFile(f)
      val lines = try source.mkString finally source.close()
      
      val uri = TurtleParser.getOne(lines, "?uri rdf:type stn:UserAccount", "uri")
      
      if (uri.isEmpty) {
        println("!!! uri not found for graph: " + lines)
      } else {
        println("Deleting: " + uri.get)
        ResourceService.repo.deleteRDFResource(uri.get)
      }
    }
    
    Ok
  }
}