package democlient


import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits.global

import org.w3.banana._
import org.w3.banana.jena._

import utils.STNParser
import utils.STNClient

    
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

  import models.STNOpsPrefix
  
  val me = Map("webid" -> "http://api.mymanufacturer.com/tvs/874...260#thing",
              STNOpsPrefix[Rdf].AgentURI.toString() -> "http://api.mymanufacturer.com/tvs/874260#thing",
              STNOpsPrefix[Rdf].SocialThingClass.toString() -> "http://www.example.com#SocialTV",
              STNOpsPrefix[Rdf].SocialThingOwner.toString() -> "http://www.example.com#John",
              STNOpsPrefix[Rdf].DisplayedName.toString() -> "John's TV"
            )
  
  
  def itCollectParams(list: List[InputParameter], collected: List[(String, String)])
          : List[(String, String)] = {
    
    if (list.isEmpty) return collected
    
    val head::tail = list
    
    if (me.contains(head.cls)) {
      itCollectParams(tail, collected :+ (head.cls, me(head.cls)))
    } else {
      // TODO if required param, request user input
      itCollectParams(tail, collected)
    }
  }
  
  def collectParams(op: Operation) = {
    itCollectParams(op.request.requiredInput ++ op.request.input, List())
  }
  
  def runOperation(stn: STNClient, opClass: String): Future[String] = {
    stn.getOperationDescription(opClass) flatMap {
      op => {
        if (!op.isEmpty) {
          val params = collectParams(op.get)
          println("registering with: " + params)
          stn.runOperation(op.get, params) map {
            response => response.body
          }
        } else {
          future { "Operation not supported:" + opClass }
        }
      }
    }
  }
  
  def run() = {
    
    println("[SocialTV] hello world !")
    
    // Retrieve platfrom specs
    val mystn = new STNClient("http://localhost:9000/assets/stnspec.ttl")

    mystn.getPlatform map {
      platform => println(platform)
    }
    
    // Create account
    runOperation(mystn, STNOpsPrefix[Rdf].CreateUserAccount toString())
  }
}