package services

object NodeService {
  
  val baseURL = "http://www.thingsnet.com"
  
  def makeUserAccountURI(id: String) = baseURL + "/users/" + id
}
