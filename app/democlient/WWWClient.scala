package democlient

import play.api.mvc._

object WWWClient extends Controller {

  def index = Action {
    Ok(html.wwwclient())
  }
}