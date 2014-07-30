package models

import services.NodeService

abstract class Resource {
  
  val uuid = NodeService.genUUID.toString
  
  def getContainer: String
  
  def getURI: String
}