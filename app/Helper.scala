package Helper

import java.awt.image
import java.io.File
import javax.imageio.ImageIO
import models.Star
import models.StarSystem
import Matrix.Matrix._
import RichStar.StarType.toRichStar
import org.squeryl.adapters.{MySQLAdapter, PostgreSqlAdapter}


class QuadTree(stars: List[StarSystem], treeDepth: Int, minX: Long, maxX: Long, minY: Long, maxY: Long) {
  require(minX < maxX)
  require(minY < maxY)
  val myStars: Option[List[StarSystem]] = if (treeDepth <= 0) Some(stars) else None
  val quadrants: List[Option[QuadTree]] = if (treeDepth <= 0)  List(None, None, None, None) else {
      val (upper, lower) = stars.partition(_.pos_x < (minX + maxX) / 2.0 )
      val (upperLeft, upperRight) = upper.partition(_.pos_y < (minY + maxY) / 2.0)
      val (lowerLeft, lowerRight) = lower.partition(_.pos_y < (minY + maxY) / 2.0)
      List(
            Some(new QuadTree(upperLeft, treeDepth - 1, minX, (minX + maxX) / 2.0 toLong, minY, (minY + maxY) / 2.0 toLong)),
            Some(new QuadTree(upperRight, treeDepth - 1, minX, (minX + maxX) / 2.0 toLong, (minY + maxY) / 2.0 toLong, maxY)),
            Some(new QuadTree(lowerLeft, treeDepth - 1, (minX + maxX) / 2.0 toLong, maxX, minY, (minY + maxY) / 2.0 toLong)),
            Some(new QuadTree(lowerRight, treeDepth - 1, (minX + maxX) / 2.0 toLong, maxX, (minY + maxY) / 2.0 toLong, maxY))
          )
    }

  def foreach[U](f: StarSystem => U) {
    myStars match {
      case Some(s) => s.foreach(f)
      case None =>
    }
    quadrants.foreach( q => q match { case Some(quad) => quad.foreach(f) case _ => })
  }

  def generatePNG(name: String, maxZoom: Int = 3, imgSize: Int = 256, curZoom: Int = 0) {
    val img = new image.BufferedImage(imgSize, imgSize, image.BufferedImage.TYPE_INT_RGB)
    foreach(starsystem => {
      val x = 1.0 * (starsystem.pos_x - minX) / (maxX - minX) * imgSize
      val y = 1.0 * (starsystem.pos_y - minY) / (maxY - minY) * imgSize

      if (!Helper.outOfBounds(x, 0, imgSize-1) && !Helper.outOfBounds(y, 0, imgSize-1))
        img.setRGB(x.toInt, y.toInt, 0xffffdea1)
    } )

    val maxCoord = 3121951219512195584l
    val x = (maxCoord + minX) * math.pow(2, (curZoom - 1).max(0)) / maxCoord + 0.01
    val y = (maxCoord + minY) * math.pow(2, (curZoom - 1).max(0)) / maxCoord + 0.01
    val file = new File("game/public/images/" + name + "z%d-x%d-y%d".format(curZoom, x.toInt, y.toInt) + ".png")

    try ImageIO.write(img, "png", file) catch {
      case e => println(e, "image saving failed")
    }
    if(curZoom < maxZoom) {
      quadrants.foreach( q => q match { case Some(quad) => WorkPool.execute { quad.generatePNG(name, maxZoom, imgSize, curZoom + 1) } case _ => })
    }
  }

}

object WorkPool {
  import scala.actors.Futures._

  val work = new scala.collection.mutable.SynchronizedQueue[() => Unit]()
  val threads = new scala.collection.mutable.SynchronizedQueue[actors.Future[Unit]]()

  val maxRunningThreads = 16


  def execute(f: => Unit) {
    work.enqueue( () => f  )
    if(threads.length < maxRunningThreads)
      runNext()
  }

  def runNext() {
    if (work.length > 0) {
      threads.enqueue( future{
        if( work.length > 0) (work.dequeue())()
        future { if (threads.length > 0) (threads.dequeue())() }
        runNext()
      })
    }
  }

  def waitForAll() {
    for(thread <- threads) {
      thread()
    }
    threads.clear()
  }
}

object Helper {
  val rnd = new scala.util.Random()

  def rndFromRange(rng: Range) = {
    rnd.nextInt(rng.last - rng.head + 1)
  }

  def nextGaussianBetween(lowerBound: Double, upperBound: Double) = {
    // Bound is 2 standard deviations
    require(lowerBound < upperBound)

    val standardDeviaton = (upperBound - lowerBound) / 4.0
    val mean = (upperBound + lowerBound) / 2.0
    nextGaussian(mean, standardDeviaton)
  }

  def roll(n: Int, sides: Int) = (for (i <- 0 until n) yield 1 + rnd.nextInt(sides)).reduceLeft(_ + _)

  def rolld6(n: Int) = roll(n, 6)

  def roll1d6() = roll(1, 6)

  def roll2d6() = roll(2, 6)

  def roll3d6() = roll(3, 6)

  def nextGaussian(mean: Double, standardDeviaton: Double) = {
    require(standardDeviaton > 0.0)

    rnd.nextGaussian() * standardDeviaton + mean
  }

  def initDB() {
    import org.squeryl._
    import play.db.DB

    Class.forName("com.mysql.jdbc.Driver")
    SessionFactory.concreteFactory = Some(() =>
      Session.create( DB.getConnection, new MySQLAdapter) )
  }

  def outOfBounds(v: Double, min: Int, max: Int) = if (v < min || v > max) true else false

  def generatePNG(name: String, starsystems: List[StarSystem],
    transformation: Matrix = identity, imgSize: Int = 256) {
    val width, height = imgSize
    val scaleFactor = (imgSize - 10) / 6e18

    val img = new image.BufferedImage(width, height,
                                       image.BufferedImage.TYPE_INT_RGB)
    for (starsystem <- starsystems) {
      val star = List(starsystem.pos_x.toDouble, starsystem.pos_y.toDouble, starsystem.pos_z.toDouble).map(_ * scaleFactor) ::: List(1.0)
      val pos = transformation * star

      val x = pos(0)
      val y = pos(1)

      if (!outOfBounds(x, 0, imgSize-1) && !outOfBounds(y, 0, imgSize-1))
        img.setRGB(x.toInt, y.toInt, 0xffffdea1)
    }
    val file = new File("game/public/images/" + name + ".png")

    try ImageIO.write(img, "png", file) catch {
      case e => println(e, "image saving failed")
    }
  }

  def generateMapTiles(starsystems: List[StarSystem]) {
    // measure how long everything takes
    import timer.Timer
    val tmr = new Timer()
    tmr.start()

    val maxCoord = 3121951219512195584l
    val stars = new QuadTree(starsystems, 7, -maxCoord, maxCoord, -maxCoord, maxCoord)
    stars.generatePNG("galaxy-map/", 6)
    WorkPool.waitForAll()

    println("took %f s to paint all tiles".format(tmr.lap()))
  }


  import org.squeryl._
  import PrimitiveTypeMode._
  import models.Game

  def getStarsystems() = {
    transaction{ (from(Game.starsystems)(s => select(s))).toList }
  }
  def displayStarsystem(id: Long) = {
    val tmr = new timer.Timer()
    tmr.start()

    transaction {
      val starsys = (from(Game.starsystems)(s => where(s.id === id) select (s))).toList.head
      val stars = (from(Game.stars)(s => where(s.starsystem_id.get === id) select (s))).toList
      val planets = (from(Game.planets)(s => where(s.star_id.get === stars(0).id) select (s))).toList

      val planetsWithMoons = for (p <- planets) yield {
        val moons = (from(Game.moons)(s => where(s.parent_planet_id === p.id) select (s))).toList
        if (moons.length == 0)
          (p, List())
        else
          (p, for (m <- moons) yield (from(Game.planets)(s => where(s.id === m.desc_planet_id) select (s))).toList.head)
      }
      println("queries took %f s".format(tmr.lap()))

       (starsys, stars, planetsWithMoons.toList.filter(p => p._1.planetType != RichPlanet.PlanetType.Empty))
    }
  }

  def getStarsystemID(mapX: Double, mapY: Double) = {
    val x = mapX * 256 / 246 * 6e18
    val y = mapY * 256 / 246 * 6e18
    val fudge = 5e15
    val candidates = transaction {
      (from(Game.starsystems)(s => where((s.pos_x.~ > x - fudge) and (s.pos_x.~ < x + fudge) and (s.pos_y.~ > y - fudge) and (s.pos_y.~ < y + fudge)) select(s))).toList
    }
    if (candidates.length > 0)
      candidates.head.id
    else
      0l
  }

  def search() = {

    transaction {
      //(from(Game.planets)(s => where(s.star_id.get === 0l) orderBy(s.id asc)).page(0, 20)).toList

    }
  }

}