package models


trait Resource {
  
  def getContainer: String
  
  def getURI: String
}