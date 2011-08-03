package controllers

import play._
import play.mvc._
import generator.Generator
import Helper.Helper
import results.Redirect


trait PasswordGuard {
  self: Controller =>
  @Before
  def check {
    session("username") match {
      case None => Secure.login
      case name => //println("Logged as %s".format(name.get))
    }
  }
}


object MainRouter extends Controller with PasswordGuard {
  Helper.initDB()

  import views.Application._
  def index = html.index("Welcome to GalaxyWar")

  def generateGalaxy = {
    Generator.generateGalaxy()
    Redirect("/")
  }

  def paintGalaxy = {
    Helper.generateMapTiles(Helper.getStarsystems())
    Redirect("/")
  }

  def emptyStarsystem = {
    html.empty()
  }

  def displayStarsystem(id: Long) = {
    id match {
      case 0 => Redirect("/empty-starsystem")
      case _ => {
        val (starsystem, stars, planetsWithMoons) = Helper.displayStarsystem(id)
        html.displayStarsystem(starsystem, stars, planetsWithMoons)
      }
    }
  }

  def matchStarsystem(mapX: Double, mapY: Double) = {
    Redirect("/starsystem?id=%d".format(Helper.getStarsystemID(mapX, mapY)))
  }


}
