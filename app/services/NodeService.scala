package services

import java.util.UUID

object NodeService {
  
  val baseURI: String = "http://localhost:9000"
  val defaultContainer: String = "/resources"
  val userAccountContainer: String = "/users"
  
  val HEADER_WebID = "X-WebID"
  
  val BAD_REQUEST_Unknown_content_type = "Unknown content type"
  
  def getPlatformURI = "http://www.thingsnet.com/#platform"
  
  def getAsset(relativePath: String) = baseURI + "/assets/" + relativePath
  
  
  def genUUID: UUID = UUID.randomUUID()
    
  def genResourceURI(container: String = defaultContainer, 
      id: String = genUUID.toString()): String = baseURI + container + "/" + id
}
