package services

import java.util.UUID

object NodeService {
  
  val baseURI: String = "http://www.thingsnet.com"
  val defaultContainer: String = "/resources"
  
  def genUUID: UUID = UUID.randomUUID()
    
  def genResourceURI(base: String = baseURI, container: String = defaultContainer, 
          id: String = genUUID.toString()): String = base + container + "/" + id
}
