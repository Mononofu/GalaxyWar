package generator

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 7/19/11
 * Time: 9:47 PM
 * To change this template use File | Settings | File Templates.
 */

import _root_.models.{Planet, Star, StarSystem, Game, Moon}
import scala.math
import Matrix.Matrix._
import timer.Timer
import collection.mutable.ListBuffer
import scala.actors.Futures._
import RichStar.StarType._
import Helper.Helper._

import org.squeryl._
import adapters.H2Adapter
import PrimitiveTypeMode._

object Generator {

  def numberOfStars() = {
    rnd.nextGaussian() match {
      case p if p < 0 => 1 // 50% of the time
      case p if p < 1.65 => 2 // about 45%
      case p => 3
    }
  }

  def testGenerators() = {
    val sampleSize = 20000
    var map = new scala.collection.mutable.HashMap[Int, Int]() {
      override def default(key: Int) = 0
    }
    for (i <- 0 to sampleSize) {
      map((determineStarMass() * 20).toInt) += 1
    }
    plotValues(map.toList, "mass_test")
  }

  def generateGalaxy() {
    //initDB()

    val starTuples = generateStars()
    val planets = generatePlanets(starTuples)
  }

  def generatePlanets(starTuples: List[(StarSystem, List[Star])]) {
    println("start of generatePlanets")
    import RichPlanet.Size._
    import RichPlanet.PlanetType._
    import RichPlanet.HelperFunctions._
    import GasGiantType._

    var planet_id = 0l
    var moon_id = 0l
    val planetBuffer = ListBuffer[Planet]()
    val moonBuffer = ListBuffer[Moon]()

    val tmr = new Timer()

    tmr.start()

    for ((starSystems, stars) <- starTuples) {
      val mainStar = stars(0)

      val closestCompanion = if (stars.length > 1) stars.drop(1)
                                                   .reduceLeft((a, b) => (
        if (a.orbitRadius < b.orbitRadius) a
        else b))
                             else mainStar
      val forbiddenZoneInner = closestCompanion.minSeperation / 3.0
      val forbiddenZoneOuter = closestCompanion.maxSeperation * 3



      if (!(forbiddenZoneInner < mainStar.innerLimitRadius &&
            forbiddenZoneOuter > mainStar.outerLimitRadius)) {
        // check if planets are possible at all

        // first gas giant
        val gasGiant = roll3d6() match {
          case p if p <= 10 => NoGasGiant
          case p if p <= 12 => Conventional
          case p if p <= 14 => Eccentric
          case _ => Epistellar
        }
        val gasGiantOrbit = gasGiant match {
          case Conventional => (roll2d6 * 0.05 + 1) * mainStar.snowLine
          case Eccentric => (roll1d6 * 0.125) * mainStar.snowLine
          case Epistellar => roll3d6 * 0.1 * mainStar.innerLimitRadius
          case _ => 0
        }

        // TODO: more sophisticated calculation involving forbidden zones
        val greatestOrbit = mainStar.outerLimitRadius
        val smallestOrbit = mainStar.innerLimitRadius
        val minOrbitSeperation = 0.15 // AU
        var orbits = List[Planet]()

        val startingOrbit = if (gasGiant != NoGasGiant) gasGiantOrbit
                            else greatestOrbit
        if (gasGiant != None) {
          orbits ::= Planet(planet_id, Some(mainStar.id), startingOrbit, GasGiant, determineGasGiantSize())
          planet_id += 1
        }


        // TODO: remove this code duplication
        //start working inward
        var curOrbit = startingOrbit / orbitSpacing()
        while (curOrbit > smallestOrbit) {
          if (isGasGiant(gasGiant, curOrbit < mainStar.snowLine)) {
            orbits ::= Planet(planet_id, Some(mainStar.id), curOrbit, GasGiant, determineGasGiantSize())
            planet_id += 1
          } else {
            // determine actual contents in second pass
            orbits ::= Planet(planet_id, Some(mainStar.id), curOrbit, Empty, Tiny)
            planet_id += 1
          }

          val newOrbit = curOrbit / orbitSpacing()
          if (curOrbit - newOrbit < minOrbitSeperation) {
            curOrbit -= minOrbitSeperation
          } else {
            curOrbit = newOrbit
          }
        }

        // work outwards now
        curOrbit = startingOrbit * orbitSpacing()
        while (curOrbit < greatestOrbit) {
          if (isGasGiant(gasGiant, curOrbit < mainStar.snowLine)) {
            orbits ::= Planet(planet_id, Some(mainStar.id), curOrbit, GasGiant, determineGasGiantSize())
            planet_id += 1
          } else {
            // determine actual contents in second pass
            orbits ::= Planet(planet_id, Some(mainStar.id), curOrbit, Empty, Tiny)
            planet_id += 1
          }
          curOrbit *= orbitSpacing()
        }

        // sorts so innermost planes are first
        orbits = orbits.sortWith((a, b) => a.orbitRadius < b.orbitRadius)

        // second pass to determine actualy type of non - gas planets
        orbits = for ((orbit, i) <- orbits.zipWithIndex) yield {
          if (orbit.planetType == GasGiant) orbit // don't touch ip it's a gas giant
          else {
            // find out what type this planet should really be
            var mod = 0

            // modifiers from gurps space p110
            // are we outermost planet ?
            if (i == orbits.length - 1) mod -= 3
            // check if next outward planet is gas giant
            else if (orbits(i + 1).planetType == GasGiant) mod -= 6

            // are we innermost planet?
            if (i == 0) mod -= 3
            //is next innward planet gas giant?
            else if (orbits(i - 1).planetType == GasGiant) mod -= 3
            // TODO: add forbidden zone modifier
            // this determines what really fills this orbit
            val contents = orbitContents(mod)
            orbit.planetType = contents._1
            orbit.size = contents._2
            orbit
          }
        }

        def orbitTimes(p: Planet, totalTidalEffect: Int = 0) {
          // we use this chance to save the orbital period
          p.orbitalPeriod = math.sqrt(math.pow(p.orbitRadius, 3) / mainStar.mass)
          val temp = roll3d6()
          p.rotationPeriod = temp + p.rotationPeriodModifier + totalTidalEffect
          if (temp >= 16 || p.rotationPeriod > 36) p.rotationPeriod = roll2d6() match {
            case 7 => roll1d6() * 2
            case 8 => roll1d6() * 5
            case 9 => roll1d6() * 10
            case 10 => roll1d6() * 20
            case 11 => roll1d6() * 50
            case 12 => roll1d6() * 100
            case _ => p.rotationPeriod
          }

          // check if we have retrograde rotation
          if (roll3d6() >= 13) p.rotationPeriod = -p.rotationPeriod
          // if so, simple make the period negative

          // TODO: check why this isn't working - commented out till then
          // check if world is tide locked
          if (totalTidalEffect >= 50 || p.rotationPeriod > 24 * 365 * p.orbitalPeriod)
            p.rotationPeriod = p.orbitalPeriod * 365 * 24
          // if it's tide locked, it can't rotate slower than it orbits the star

          // determine axial tilt

          p.axialTilt = roll3d6() match {
            case 3 | 4 | 5 | 6 => roll2d6() - 2
            case 7 | 8 | 9 => 10 + roll2d6() - 2
            case 10 | 11 | 12 => 20 + roll2d6() - 2
            case 13 | 14 => 30 + roll2d6() - 2
            case 15 | 16 => 40 + roll2d6() - 2
            case 17 | 18 => roll1d6() match {
              case 1 | 2 => 50 + roll2d6() - 2
              case 3 | 4 => 60 + roll2d6() - 2
              case 5 => 70 + roll2d6() - 2
              case 6 => 80 + roll2d6() - 2
            }
          }
        }

        def fleshOutTerrestrialPlanet(p: Planet, majorMoons: Int = 0) {
          p.blackbodyT = (278 * math.pow(mainStar.luminosity / mainStar.sunLuminosity, 0.25) / math.sqrt(p.orbitRadius)) toInt;
          p.planetType = p.terrestrialType(mainStar.mass, mainStar.age)

          // atmoshphere
          p.atmosphericMass = roll3d6() / 10.
          val (suff, corr, tox, marg) = p.determineAtmosphericComposition()
          p.suffocating = suff
          p.corrosive = corr
          p.toxic = tox
          p.marginalAtmosphere = marg


          p.hydrographicCoverage = p.determineHydrographicCoverage()

          p.density = p.determineTerrestrialDensity()
          p.radius = p.determineTerrestrialRadius()


          import RichPlanet.VolcanicActivity._
          p.volcanicActivity = (roll3d6() + (p.gravity / (mainStar.age / 1e9)) * 40 + (5 * majorMoons).min(10)) match {
            case t if t <= 16 => NoActivity
            case t if t <= 20 => LightActivity
            case t if t <= 26 => ModerateActivity
            case t if t <= 70 => HeavyActivity
            case _ => ExtremeActivity
          }

          if (p.size == Tiny || p.size == Small)
            p.tectonicActivity = NoActivity
          else
            p.tectonicActivity = roll3d6() + (4 * p.volcanicActivity.id) - 8 + (p.hydrographicCoverage * 2).toInt + 2 * majorMoons.min(2) match {
              case p if p <= 6 => NoActivity
              case p if p <= 10 => LightActivity
              case p if p <= 14 => ModerateActivity
              case p if p <= 18 => HeavyActivity
              case _ => ExtremeActivity
            }

          p.resourceValue = if (p.planetType == AsteroidBelt) roll3d6() match {
            case 3 => -5
            case 4 => -4
            case 5 => -3
            case 6 | 7 => -2
            case 8 | 9 => -1
            case 10 | 11 => 0
            case 12 | 13 => 1
            case 14 | 15 => 2
            case 16 => 3
            case 17 => 4
            case 18 => 5
          } else (roll3d6() + p.volcanicActivity.id - 2) match {
            case p if p <= 2 => -3
            case 3 | 4 => -2
            case 5 | 6 | 7 => -1
            case 8 | 9 | 10 | 11 | 12 | 13 => 0
            case 14 | 15 | 16 => 1
            case 17 | 18 => 2
            case _ => 3
          }

        }

        for (p <- orbits) {
          if (p.planetType == GasGiant) {
            p.density = p.determineGasGiantDensity()
            p.radius = (math.pow(p.determineGasGiantMass() / p.density, 0.333) * 6378).toInt
          }
        }


        // now we determine which and how many moons each planet has
        for (p <- orbits) {
          // returs for each planet the number of moons and rings
          // in the format: (rings: Int, majorMoons: List[Size], moonlets: Int)
          val moons = p.determineMoons()

          if (p.planetType == Terrestrial)
            fleshOutTerrestrialPlanet(p, moons._2.length)


          // tidal force of the main star on the planet
          var tidalForceSum = (0.46 * mainStar.mass * p.radius / 6378.) / math.pow(p.orbitRadius, 3)

          p.rings = moons._1
          for (moonSize <- moons._2) {
            // in km
            val orbitRadius = p.radius * (if (p.planetType == GasGiant) {
              val temp = roll3d6() + 3
              if (temp >= 15) temp + roll2d6() else temp
            } else (roll2d6() + (8 - (p.size.id - moonSize.id) * 2)) * 5)

            val moon = Planet(planet_id, Some(0), p.orbitRadius, Terrestrial, moonSize)
            fleshOutTerrestrialPlanet(moon)
            orbitTimes(moon, ((17.8e6 * p.mass * moon.radius / 6378.) * math.pow(moon.orbitRadius / 6378., 3)).toInt)
            moon.moonlets = orbitRadius // abused to store orbit radius of moons
            moon.orbitalPeriod = 1.611e-4 * math.sqrt(math.pow(orbitRadius / 6378., 3) / (moon.mass * p.mass))

            planetBuffer += moon
            moonBuffer += Moon(moon_id, Some(p.id), Some(moon.id))
            planet_id += 1
            moon_id += 1

            // tidal forces in multiples of the one exerted on earth
            tidalForceSum += (17.8e6 * moon.mass * p.radius / 6378.) * math.pow(moon.orbitRadius / 6378., 3)
          }
          p.moonlets = moons._3

          val tot = ((tidalForceSum * (mainStar.age / 1e9)) / p.mass).toInt
          orbitTimes(p, tot)

        }

        planetBuffer ++= orbits

        if (planetBuffer.length > 50000) {
          val planetList = planetBuffer.toList
          val moonList = moonBuffer.toList
          //val sqlActor = future {
            println("Starting future to save planets to db")
            println("%d planets".format(planetList.length))
            println("%d moons".format(moonList.length))

          try {
            inTransaction {
              import models.Game

              Game.planets.insert(planetList)
              Game.moons.insert(moonList)
            }
          }
          catch {
            case ex: java.sql.BatchUpdateException =>
              throw ex.getNextException()
          }
          //}
          planetBuffer.clear()
          moonBuffer.clear()
        }
      }
    }

    println("took %f to generate planets".format(tmr.lap()))

    println("Starting future to save planets to db")
    println("%d planets".format(planetBuffer.length))
    println("%d moons".format(moonBuffer.length))

    inTransaction {
      import models.Game

      Game.planets.insert(planetBuffer.toList)
      Game.moons.insert(moonBuffer.toList)
    }


  }


  var starSystemsPk = 0l
  var starPk        = 0l

  import NameGenerator.NameGenerator

  val gen = new NameGenerator()
  gen.load("src/main/resources/starnames.data")

  def saveStarsystem(x: Double, y: Double, z: Double) = {
    starSystemsPk += 1
    starPk += 1
    var stars = List(Star(starPk, Some(starSystemsPk), determineStarMass(),
                           determineStarAge(), fullname = gen.generateWord(5, 20).capitalize))
    var otherMass = stars(0).mass // mass of the other stars
    for (i <- 1 until numberOfStars()) {
      // iterate for numberOfStars-1 times (first star above)
      val orbitRadius = determineOrbitRadius()
      val ecc = determineOrbitEccentricity(orbitRadius)
      val orbitPeriod = math.sqrt(math.pow(orbitRadius, 3) / otherMass)
      val mass = determineStarMass()

      otherMass += mass // next star will orbit all previous stars

      starPk += 1
      stars ::=
      Star(starPk, Some(starSystemsPk), mass, determineStarAge(),
            orbitRadius, ecc, orbitPeriod, gen.generateWord(5, 20).capitalize)

    }
    (StarSystem(starSystemsPk, x.toLong, y.toLong, z.toLong), stars)
  }

  def generateStars(numCore: Int = 60000, numDisk: Int = 50000,
    coreRadius: Double = 5e17, diskRadius: Double = 2e18,
    coreMaxZ: Double = 3.5e17, diskMaxZ: Double = 1e17,
    numArms: Int = 4, armRots: Double = 0.5,
    armWidth: Double = 45, fuzz: Double = 20,
    seed: Int = 122132) = {

    val tmr = new Timer()

    tmr.start()

    rnd.setSeed(seed)

    starSystemsPk = 0l

    val stars = ListBuffer((StarSystem(starSystemsPk, 0, 0, 0, "Center of Galaxy"),
      List(Star(starPk, Some(starSystemsPk), 1000, 10e10.toLong, fullname = "Black Hole"))))
    val omega = if (numArms > 0) 360.0 / numArms else 0.0

    // generate stars in the arms of the galaxy
    for (i <- 1 to numDisk) {
      var gaussFact = math.abs(nextGaussian(0.0, 0.6))
      while (gaussFact > 1.2) {
        gaussFact = math.abs(nextGaussian(0.0, 0.6))
      }
      val dist = coreRadius + gaussFact * diskRadius
      val theta = (360.0 * armRots * (dist / diskRadius)
                   + rnd.nextDouble * armWidth
                   + omega * rnd.nextInt(numArms)
                   + fuzz * 2.0 * rnd.nextDouble - fuzz)

      val x = math.cos(theta * math.Pi / 180.0) * dist
      val y = math.sin(theta * math.Pi / 180.0) * dist
      val z = (rnd.nextDouble * 2.0 - 1) *
              (diskMaxZ - diskMaxZ / math.pow(diskRadius +
                                              coreRadius, 2) *
                          dist * dist)

      stars += saveStarsystem(x, y, z)
    }

    val tDisk = tmr.lap()

    // generate the galaxy's core
    val scale = coreMaxZ / (math.pow(coreRadius, 3) * 24)
    for (i <- 1 to numCore) {
      var gaussFact = math.abs(nextGaussian(0.0, 1.0))
      while (gaussFact > 2.2) {
        gaussFact = math.abs(nextGaussian(0.0, 1.0))
      }
      val dist = coreRadius * 1.3 * gaussFact
      val theta = 360.0 * rnd.nextDouble

      val x = math.cos(theta * math.Pi / 180.0) * dist
      val y = math.sin(theta * math.Pi / 180.0) * dist
      val z = (rnd.nextDouble * 2.0 - 1) *
              (coreMaxZ - scale * math.pow(dist, 3)) / 4

      stars += saveStarsystem(x, y, z)
    }

    // correct for the fact the just between the edge of the core and the beginning of the disk,
    // there are far to few stars
    for (i <- 1 to numCore / 4) {
      var gaussFact = math.abs(nextGaussian(0.0, 1.0))
      while (gaussFact > 1.0) {
        gaussFact = math.abs(nextGaussian(0.0, 1.0))
      }
      val dist = coreRadius * (1 - gaussFact)
      val theta = 360.0 * rnd.nextDouble

      val x = math.cos(theta * math.Pi / 180.0) * dist
      val y = math.sin(theta * math.Pi / 180.0) * dist
      val z = (rnd.nextDouble() * 2 - 1) * coreMaxZ * (1 - math.pow(dist, 3) /
                                                           math
                                                           .pow(coreRadius, 3))

      stars += saveStarsystem(x, y, z)
    }

    val starlist = stars.toList

    val sqlActor = future {
      println("Starting future to save stars to db")
      println("%d stars".format(stars.length))

      inTransaction {
        import models.Game._

        drop
        create
        val starsystems = for (ss <- starlist) yield ss._1
        val stars = (List[Star]() /: starlist)((total, starsys) => starsys._2 ::: total)

        Game.starsystems.insert(starsystems)
        Game.stars.insert(stars)
      }
    }

    starlist
  }


}