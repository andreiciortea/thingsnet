package models


abstract class Resource {
  
  def getContainer: String
  
  def getURI: String
}