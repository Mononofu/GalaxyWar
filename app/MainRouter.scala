package controllers

import play._
import play.mvc._
import generator.Generator
import Helper.Helper

object MainRouter extends ScalateController {
  Helper.initDB()

  import views.Application._

  def index = html.index("Welcome to GalaxyWar")
  def generateGalaxy = {
    Generator.generateGalaxy()
    index
  }
  def paintGalaxy = {
    Helper.generateMapTiles(Helper.getStarsystems())
    index
  }
  def displayGalaxy = {
    render("templates/display-galaxy.jade")
  }
  def testGenerators =  {
    Generator.testGenerators()
    render("templates/generators-test.jade")
  }
  def displayStarsystem(id: Long) = {
    val (starsystem, stars, planetsWithMoons) = Helper.displayStarsystem(id)
    html.displayStarsystem(starsystem, stars, planetsWithMoons)
  }

  def matchStarsystem(mapX: Double, mapY: Double) = {
    Helper.getStarsystemID(mapX, mapY)
  }


}
