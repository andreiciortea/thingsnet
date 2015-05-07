package utils

import java.net.URLDecoder
import org.apache.commons.validator.routines.UrlValidator

object Validator {
  
  def isValidUri(uri: String): Boolean = {
    (new UrlValidator(List("http", "https").toArray).isValid(URLDecoder.decode(uri,"UTF-8")))
  }
}