package democlient


import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.w3.banana._
import org.w3.banana.jena._

import play.api.libs.ws._

import utils._

    
/**
 * The social TV is a software agent for our IoT15 application scenario.
 * 
 * retrieve John's STN Box's description
 * create an account
 *    check input params
 *    run operation
 *    check result
 * retrieve John's profile and extract connections to friends on other boxes
 * for each friend
 *    retrieve the STN desc of his box
 *    retrieve his profile and extract connections to all other devices
 *    check each device for its class
 * 
 */


import utils.InputParameter
import utils.Operation


object SocialTV extends RDFModule with JenaModule {

  import Ops._
  import models.STNPrefix
  import models.STNOpsPrefix
  
  // TODO better kb management
  val me = Map("webid" -> "http://api.mymanufacturer.com/tvs/874...260#thing",
              STNOpsPrefix[Rdf].AgentURI.toString() -> "http://api.mymanufacturer.com/tvs/874260#thing",
//              STNOpsPrefix[Rdf].AgentURI.toString() -> "http://www.example.com/#thing1",
              STNOpsPrefix[Rdf].SocialThingClass.toString() -> "http://www.example.com#SocialTV",
              STNOpsPrefix[Rdf].SocialThingOwner.toString() -> "http://www.example.com#John",
              STNOpsPrefix[Rdf].DisplayedName.toString() -> "John's TV"
//              STNOpsPrefix[Rdf].UserAccountURI.toString() -> "http://localhost:9000/users/791dfe04-53cd-4dbd-af0b-ff51d8b85412"
            )
  
  
  def itCollectParams(list: List[InputParameter], params: Map[String, String], 
      collected: List[(InputParameter, String)]) : List[(InputParameter, String)] = {
    
    if (list.isEmpty) return collected
    
    val head::tail = list
    
    val value = params.get(head.cls)
    if (!value.isEmpty) {
      itCollectParams(tail, params, collected :+ (head, value.get))
    } else {
      // TODO if required param, request user input
      itCollectParams(tail, params, collected)
    }
  }
  
  def collectParams(op: Operation, params: Map[String, String]) = {
    itCollectParams(op.request.requiredInput ++ op.request.input, params, List())
  }
  
  def isOperationSupported(stn: STNClient, opClass: String): Future[Boolean] = {
    stn.getOperationDescription(opClass) map {
      op => op.isEmpty
    }
  }
  
  def runOperation(stn: STNClient, opClass: String, params: Map[String, String]): Future[String] = {
    stn.getOperationDescription(opClass) flatMap {
      op => {
        if (!op.isEmpty) {
          val collectedParams = collectParams(op.get, params)
          stn.runOperation(op.get, collectedParams) map {
            response => response.body
          }
        } else {
          Future { "Operation not supported:" + opClass }
        }
      }
    }
  }
  
  def retrieveOutConnections(stn: STNClient, accountUri: String) = {
    runOperation(stn, 
        STNOpsPrefix[Rdf].GetOutConnections.toString(),
        Map(STNOpsPrefix[Rdf].UserAccountURI.toString() -> accountUri)
      )
  }
  
  def retrieveProfileAndExtractConnections(stn: STNClient, agentUri: String) = {
    val result = runOperation(stn,
        STNOpsPrefix[Rdf].WhoIsAgent.toString(),
        Map(STNOpsPrefix[Rdf].AgentURI.toString() -> agentUri)
      )
    
    result map {
      graph =>
        val accountUri = 
          TurtleParser.getOne(graph, "?accountUri a stn:UserAccount ; stn:heldBy ?agentUri", 
              "accountUri", Map("?agentUri" -> URI(agentUri)))
        
        println("John's account URI: " + accountUri)
        
        if (!accountUri.isEmpty) {
          TurtleParser.getListOfOne(graph, "?accountUri stn:connectedTo ?connectionUri", 
              "connectionUri", Map("?accountUri" -> URI(accountUri.get)))
        } else {
          None
        }
    }
  }
  
  def fetchAndExtractThings(stn: STNClient, accountUri: String) = {
    println("Retrieving account for: " + accountUri)
    val result = stn.fetchURL(accountUri)
    
    result map {
      response =>
          val things = TurtleParser.getListOfOne(response.body, "?accountUri stn:heldBy [ stn:owns ?thingUri ]", 
              "thingUri", Map("?accountUri" -> URI(accountUri)))
          println("Found things: " + things)
          things
    }
  }
  
  def selectSocialTVs(stn: STNClient, things: List[String], found: List[String]): Future[List[String]] = {
    if (things.isEmpty) Future { found }
    else {
      val head::tail = things
      stn.fetchURL(head) flatMap {
        result =>
          if (TurtleParser.getOne(result.body, 
              "?thingUri a <http://www.example.com/#SocialTV>", "thingUri").isEmpty) {
            selectSocialTVs(stn, tail, found)
          } else {
            selectSocialTVs(stn, tail, found :+ head)
          }
      }
    }
  }
  
  def discoverTVs(stn: STNClient, accounts: List[String], discovered: List[String]): Future[List[String]] = {
    if (accounts.isEmpty) {
      Future { discovered }
    } else {
      val head::tail = accounts
      fetchAndExtractThings(stn, head) flatMap {
        results =>
          if (results.isEmpty) discoverTVs(stn, tail, discovered)
          else {
            selectSocialTVs(stn, results.get.toList, List()) flatMap {
              tvs => discoverTVs(stn, tail, discovered ++ tvs)
            }
          }
      }
    }
  }
  
  def run(): Future[String] = {
    
    import utils.TurtleParser
    
    println("[SocialTV] hello world !")
    
    // Retrieve platfrom specs
    val mystn = new STNClient("http://localhost:9000/assets/stnspecs/thingsnet.ttl")

    mystn.getPlatform map {
      platform => println(platform)
    }
    
    // Create account
/*    runOperation(mystn, 
        STNOpsPrefix[Rdf].CreateUserAccount toString(),
        Map(STNOpsPrefix[Rdf].SocialThingClass.toString() -> "http://www.example.com/#SocialTV",
          STNOpsPrefix[Rdf].SocialThingOwner.toString() -> "http://www.example.com/#John",
          STNOpsPrefix[Rdf].DisplayedName.toString() -> "John's TV")
      )*/
    
    retrieveProfileAndExtractConnections(mystn, "http://www.example.com/#John") flatMap {
      connections =>
        discoverTVs(mystn, connections.get.toList, List()) map {
          list => list.toString()
        }
    }
  }
}