package controllers

import play.api.mvc._

object Application extends Controller {
  
  def index = Action {
    import java.util.Date
    println(new Date("Nov 4, 2003, 8:14 PM"))
    Ok(views.html.index())
  }
  
}