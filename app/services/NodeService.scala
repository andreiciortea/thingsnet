package services

import java.util.UUID

object NodeService {
  
  val baseURI: String = "http://localhost:9000"
  val defaultContainer: String = "/resources"
  
  def getPlatformURI = baseURI + "/#platform"
  
  def genUUID: UUID = UUID.randomUUID()
    
  def genResourceURI(base: String = baseURI, container: String = defaultContainer, 
          id: String = genUUID.toString()): String = base + container + "/" + id
  
  def getAsset(relativePath: String) = baseURI + "/assets/" + relativePath
}
