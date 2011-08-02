package Helper

import java.awt.image
import java.io.File
import javax.imageio.ImageIO
import models.Star
import models.StarSystem
import Matrix.Matrix._
import RichStar.StarType.toRichStar
import org.squeryl.adapters.{MySQLAdapter, PostgreSqlAdapter}

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
    println("start of paintGalaxy")
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
import scala.actors.Futures._

    val painters = scala.collection.mutable.Queue[actors.Future[Unit]]()
    val threadCount = 16

    // measure how long everything takes
    import timer.Timer
    val tmr = new Timer()
    tmr.start()

    // split into 4 quadrants for improved performance
    val (upper, lower) = starsystems.partition(_.pos_x < 0)
    val (upperLeft, upperRight) = upper.partition(_.pos_y < 0)
    val (lowerLeft, lowerRight) = lower.partition(_.pos_y < 0)
    val quadrants = List(List(upperLeft, upperRight), List(lowerLeft, lowerRight))

    // paint single image of all stars
    painters += future{ generatePNG("galaxy-map/z0-x0-y0", starsystems, identity, 256) }


    // now paint all the other tiles
    val maxZoomLevel = 7
    for (i <- 1 until maxZoomLevel) {
      val zoom = math.pow(2, i).toInt
      for (x <- 0 until zoom; y <- 0 until zoom) {
        val filename = ("galaxy-map/" +
                        "z%d-x%d-y%d").format(i, x, y)
        val trans = identity.translate(128 * zoom - 256 * x , 128 * zoom - 256 * y).scale(zoom, zoom)
        val point = List(64.0, -32.0, 10.0, 1.0)

        if (painters.length > threadCount) {
          (painters.dequeue())()
        }

        // determine which quadrant we are in
        painters += future{ generatePNG(filename, quadrants(2*x/zoom)(2*y/zoom), trans, 256) }

      }
    }
    
    for(p <- painters)
      p()

    println("took %f s to paint all tiles".format(tmr.lap()))
  }


  def plotValues(vals: Traversable[(Int, Int)], filename: String,
    width: Int = 800, height: Int = 800) {
    val img = new image.BufferedImage(width, height,
                                       image.BufferedImage.TYPE_INT_RGB)

    val (xMax, yMax) = vals.foldLeft((0, 0))((x, y) => (x._1 max y._1, x._2 max
                                                                       y._2))
    val (xMin, yMin) = vals
                       .foldLeft((100, 100))((x, y) => (x._1 min y._1, x._2 min
                                                                       y._2))
    println("%d : %d".format(xMax, yMax))
    println("%d : %d".format(xMin, yMin))


    for (v <- vals) {
      val x = (1.0 * v._1 / xMax * width - 3 toInt).max(0) + 1
      val y = (height - 2) - ((1.0 * v._2 / yMax * height - 2 toInt).max(0))
      //println("%d : %d".format(x, y))
      img.setRGB(x, y, 0xffffffff)
      img.setRGB(x + 1, y, 0xffffffff)
      img.setRGB(x, y + 1, 0xffffffff)
      img.setRGB(x + 1, y + 1, 0xffffffff)
    }

    img.setRGB((20.0 / xMax * width - 3 toInt).max(0) + 1, 20, 0xffff00ff)
    img.setRGB((20.0 / xMax * width - 3 toInt).max(0) + 1, 21, 0xffff00ff)
    img.setRGB((20.0 / xMax * width - 3 toInt).max(0) + 2, 20, 0xffff00ff)
    img.setRGB((20.0 / xMax * width - 3 toInt).max(0) + 2, 21, 0xffff00ff)

    img.setRGB((40.0 / xMax * width - 3 toInt).max(0) + 1, 20, 0xffff00ff)
    img.setRGB((40.0 / xMax * width - 3 toInt).max(0) + 1, 21, 0xffff00ff)
    img.setRGB((40.0 / xMax * width - 3 toInt).max(0) + 2, 20, 0xffff00ff)
    img.setRGB((40.0 / xMax * width - 3 toInt).max(0) + 2, 21, 0xffff00ff)

    val file = new File("src/main/webapp/images/" + filename + ".png")
    try ImageIO.write(img, "png", file) catch {
      case e => println(e, "image saving failed")
    }
  }

  import org.squeryl._
  import PrimitiveTypeMode._
  import models.Game

  def generateJSON(filename: String, starsystems: List[StarSystem],
    transformation: Matrix = identity, imgSize: Int = 256) {
    println("start of generateJSON")

    val fstream = new java.io.FileWriter("game/public/images/" + filename + ".json")
    val out = new java.io.BufferedWriter(fstream)
    val scaleFactor = (imgSize - 10) / 5e18
    
    out.write((for(starsystem <- starsystems) yield {
      val star = List(starsystem.pos_x.toDouble, starsystem.pos_y.toDouble, starsystem.pos_z.toDouble).map(_ * scaleFactor) ::: List(1.0)
      val pos = transformation * star
      if (!outOfBounds(pos(0), 0, imgSize-1) && !outOfBounds(pos(1), 0, imgSize-1))
        Some("""{"type": "Feature", "geometry": { "type": "Point", "coordinates": [%f, %f] }, "properties": {} }""".format(pos(0), pos(1) ) )
      else
        None
    }).flatten.mkString("""{"type": "FeatureCollection", "features": [""", ",", "]}"))
    out.close()
  }

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
    println("%f:%f".format(x, y))
    val fudge = 5e15
    val candidates = transaction {
      (from(Game.starsystems)(s => where((s.pos_x.~ > x - fudge) and (s.pos_x.~ < x + fudge) and (s.pos_y.~ > y - fudge) and (s.pos_y.~ < y + fudge)) select(s))).toList
    }
    println("candidates")
    println(candidates)
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