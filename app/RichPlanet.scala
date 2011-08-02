package RichPlanet

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 7/2/11
 * Time: 9:57 PM
 * To change this template use File | Settings | File Templates.
 */

import Helper.Helper._
import models.Planet

object PlanetType extends Enumeration {
  type PlanetType = Value
  val GasGiant         = Value("Gas giant")
  val Terrestrial      = Value("Terrestrial")
  val IcePlanet        = Value("Ice")
  val RockPlanet       = Value("Rock")
  val DwarfPlanet      = Value("Dwarf")
  val HadeanPlanet     = Value("Hadean")
  val AmmoniaPlanet    = Value("Ammonia")
  val GardenPlanet     = Value("Garden")
  val OceanPlanet      = Value("Ocean")
  val GreenhousePlanet = Value("Greenhouse")
  val ChtonianPlanet   = Value("Chtonian")
  val AsteroidBelt     = Value("Asteroids")
  val Empty            = Value("empty")

  implicit def toRichPlanet(p: Planet): RichPlanet = new RichPlanet(p)
}

object Size extends Enumeration {
  type Size = Value
  val Tiny   = Value(0, "tiny")
  val Small  = Value(1, "small")
  val Medium = Value(2, "medium")
  val Large  = Value(3, "large")

  def determineGasGiantSize() = {
    roll3d6 match {
      case p if p <= 10 => Small
      case p if p <= 16 => Medium
      case _ => Large
    }
  }
}

object AtmosphericPressure extends Enumeration {
  type AtmosphericPressure = Value
  val Trace      = Value("trace")
  val VeryThin   = Value("very thin")
  val Thin       = Value("thin")
  val Standard   = Value("standard")
  val Dense      = Value("dense")
  val VeryDense  = Value("very dense")
  val Superdense = Value("superdense")
}

object Toxic extends Enumeration {
  type Toxic = Value
  val NotToxic      = Value("not toxic")
  val MildlyToxic   = Value("mildly toxic")
  val HighlyToxic   = Value("highly toxic")
  val LethallyToxic = Value("lethally toxic")
}

object MarginalAtmosphere extends Enumeration {
  type MarginalAtmosphere = Value
  val NotMarginal   = Value("None")
  val Chlorine      = Value("Chlorine")
  val SulfurComp    = Value("Sulfur Compounds")
  val NitrogenComp  = Value("Nitrogen Compounds")
  val OrganicToxins = Value("Organic Toxins")
  val LowOxygen     = Value("Low Oxygen")
  val Pollutants    = Value("Pollutants")
  val HighCO2       = Value("High Carbon Dioxide")
  val HighOxygen    = Value("High Oxygen")
  val InertGases    = Value("Inert Gases")
}

object ClimateType extends Enumeration {
  type ClimateType = Value
  val Frozen   = Value("Frozen") 
  val VeryCold = Value("Very Cold")
  val Cold     = Value("Cold")
  val Chilly   = Value("Chilly")
  val Cool     = Value("Cool")
  val Normal   = Value("Normal")
  val Warm     = Value("Warm")
  val Tropical = Value("Tropical")
  val Hot      = Value("Hot")
  val VeryHot  = Value("Very Hot")
  val Infernal = Value("Infernal")
}

object VolcanicActivity extends Enumeration {
  type VolcanicActivity = Value
  val NoActivity       = Value("none")
  val LightActivity    = Value("light")
  val ModerateActivity = Value("moderate")
  val HeavyActivity    = Value("heavy")
  val ExtremeActivity  = Value("extreme")
}

object HelperFunctions {

  object GasGiantType extends Enumeration {
    type GasGiantType = Value
    val NoGasGiant   = Value("none")
    val Conventional = Value("conventional")
    val Eccentric    = Value("eccentric")
    val Epistellar   = Value("epistellar")
  }

  import GasGiantType._

  def isGasGiant(arrangement: GasGiantType, insideSnowLine: Boolean) = {
    arrangement match {
      case NoGasGiant => false
      case Conventional => if (insideSnowLine) false
                           else roll3d6() <= 15
      case Eccentric => if (insideSnowLine) roll3d6() <= 8
                        else roll3d6() <= 14
      case Epistellar => if (insideSnowLine) roll3d6() <= 6
                         else roll3d6() <= 14
    }
  }

  import PlanetType._
  import Size._

  def orbitContents(mod: Int) = {
    (roll3d6 + mod) match {
      case p if p <= 3 => (Empty, Medium)
      case p if p <= 6 => (AsteroidBelt, Medium)
      case p if p <= 8 => (Terrestrial, Tiny)
      case p if p <= 9 => (Terrestrial, Small)
      case p if p <= 13 => (Terrestrial, Medium)
      case _ => (Terrestrial, Large)
    }
  }


  def orbitSpacing() = {
    roll3d6 match {
      case p if p <= 4 => 1.4
      case p if p <= 6 => 1.5
      case p if p <= 8 => 1.6
      case p if p <= 12 => 1.7
      case p if p <= 14 => 1.8
      case p if p <= 16 => 1.9
      case _ => 2.0
    }
  }


}

import PlanetType._
import Size._

case class RichPlanet(p: Planet) {
  private def gasGiantRingMod = p.orbitRadius match {
    case p if p <= 0.1 => -10
    case p if p <= 0.5 => -8
    case p if p <= 0.75 => -6
    case p if p <= 1.5 => -3
    case _ => 0
  }

  private def gasGiantMajorMoonMod = p.orbitRadius match {
    case p if p <= 0.1 => -6
    case p if p <= 0.5 => -5
    case p if p <= 0.75 => -4
    case p if p <= 1.5 => -1
    case _ => 0
  }

  private def gasGiantMinorMoonMod = {
    if (p.orbitRadius < 3) gasGiantMajorMoonMod - 1
    else 0
  }

  private def terrestrialMoonMod = {
    (p.orbitRadius match {
      case p if p <= 0.5 => -4 // no moons for you
      case p if p <= 0.75 => -3
      case p if p <= 1.5 => -1
      case _ => 0
    }) +
    (p.size match {
      case Tiny => -2
      case Small => -1
      case Large => +1
      case _ => 0
    })
  }

  private def moonSize = p.planetType match {
    case GasGiant => Size(Large.id + moonSizeMod)
    case _ => Size((p.size.id + moonSizeMod).max(0))
  }

  private def moonSizeMod = roll3d6 match {
    case p if p <= 11 => -3
    case p if p <= 14 => -2
    case _ => -1
  }

  def determineMoons(): (Int, List[Size.Value], Int) = {
    p.planetType match {
      // which type of planet do we have?
      case GasGiant => (roll2d6 + gasGiantRingMod,
        (for (i <- 0 until roll1d6 + gasGiantMajorMoonMod) yield moonSize).toList,
        (roll1d6 + gasGiantMinorMoonMod).max(0))
      case Terrestrial => {
        val majorMoons = roll1d6() - 4 + terrestrialMoonMod
        if (majorMoons > 0) {
          (0, (for (i <- 0 until majorMoons) yield moonSize).toList, 0)
        }
        else (0, List(), (roll1d6() - 2 + terrestrialMoonMod).max(0))
      }
      case _ => (0, List(), 0) // no moons if there's no planet
    }
  }

  def terrestrialType(starMass: Double, starAge: Long) = {
    val t = p.blackbodyT
    p.size match {
      case Tiny => if (t < 141) IcePlanet else RockPlanet
      case Small => if (t < 81) HadeanPlanet else if (t < 141) IcePlanet else RockPlanet
      case Medium => t match {
        case t if t <= 80 => HadeanPlanet
        case t if t <= 150 => IcePlanet
        case t if t <= 230 => if (starMass < 0.65) AmmoniaPlanet else IcePlanet
        case t if t <= 240 => IcePlanet
        case t if t <= 320 => if (roll3d6() + (starAge / 5e8).min(10) >= 18) GardenPlanet else OceanPlanet
        case t if t <= 500 => GreenhousePlanet
        case _ => ChtonianPlanet
      }
      case Large => t match {
        case t if t <= 150 => IcePlanet
        case t if t <= 230 => if (starMass < 0.65) AmmoniaPlanet else IcePlanet
        case t if t <= 240 => IcePlanet
        case t if t <= 320 => if (roll3d6() + (starAge / 5e8).min(5) >= 18) GardenPlanet else OceanPlanet
        case t if t <= 500 => GreenhousePlanet
        case _ => ChtonianPlanet
      }
    }
  }


  import MarginalAtmosphere._

  def determineMarginalAtmosphere() = roll3d6() match {
    case p if p <= 4 => Chlorine
    case p if p <= 6 => SulfurComp
    case p if p <= 7 => NitrogenComp
    case p if p <= 9 => OrganicToxins
    case p if p <= 11 => LowOxygen
    case p if p <= 13 => Pollutants
    case p if p <= 14 => HighCO2
    case p if p <= 16 => HighOxygen
    case _ => InertGases
  }

  def determineAtmosphericComposition() = {
    import Toxic._
    var suffocating = true
    var corrosive = false
    var toxic = NotToxic
    var marginalAtmosphere = NotMarginal
    p.size match {
      case Small => if (p.planetType == IcePlanet) {
        toxic = if (roll3d6() <= 15) MildlyToxic else HighlyToxic
      }
      case Medium => p.planetType match {
        case AmmoniaPlanet =>
          toxic = LethallyToxic
          corrosive = true
        case IcePlanet =>
          if (roll3d6() <= 12) toxic = MildlyToxic
        case OceanPlanet =>
          if (roll3d6() <= 12) toxic = MildlyToxic
        case GardenPlanet =>
          suffocating = false
          if (roll3d6() >= 12) marginalAtmosphere = determineMarginalAtmosphere()
        case GreenhousePlanet =>
          toxic = LethallyToxic
          corrosive = true
        case _ =>
      }
      case Large => p.planetType match {
        case AmmoniaPlanet =>
          toxic = LethallyToxic
          corrosive = true
        case IcePlanet =>
          toxic = HighlyToxic
        case OceanPlanet =>
          toxic = HighlyToxic
        case GardenPlanet =>
          suffocating = false
          if (roll3d6() >= 12) marginalAtmosphere = determineMarginalAtmosphere()
        case GreenhousePlanet =>
          toxic = LethallyToxic
          corrosive = true
        case _ =>
      }
      case _ =>
    }
    (suffocating, corrosive, toxic, marginalAtmosphere)
  }

  // percent of surface covered, 0 to 1
  def determineHydrographicCoverage() = p.planetType match {
    case IcePlanet => p.size match {
      case Small => (roll1d6() + 2) / 10.
      case Medium => (roll2d6() - 10).max(0) / 10.
      case Large => (roll2d6() - 10).max(0) / 10.
      case _ => 0.
    }
    case AmmoniaPlanet => roll2d6() / 10.
    case OceanPlanet => (roll1d6() + 6).min(10) / 10.
    case GardenPlanet => (roll1d6() + 4) / 10.
    case GreenhousePlanet => (roll2d6() - 7).max(0) / 10.
    case _ => 0.
  }


  // returns denisty relative to earth
  def determineTerrestrialDensity() = {
    val density = roll3d6() match {
      case p if p <= 6 => 0.3
      case p if p <= 10 => 0.4
      case p if p <= 14 => 0.5
      case p if p <= 17 => 0.6
      case _ => 0.7
    }
    if (p.planetType == RockPlanet) density + 0.3
    else if (p.planetType == AmmoniaPlanet || p.size == Tiny || p.size == Small) density
    else density + 0.5
  }

  // returns planet radius in km
  def determineTerrestrialRadius() = {
    val minDia = p.size match {
      case Tiny => 0.004
      case Small => 0.024
      case Medium => 0.030
      case Large => 0.065
    }
    val range = p.size match {
      case Tiny => 0.020
      case Small => 0.006
      case Medium => 0.035
      case Large => 0.026
    }
    (math.sqrt(p.blackbodyT / p.density) * (minDia + (roll2d6() - 2) * range / 10) * 6378).toInt
  }

  def determineGasGiantMassDensity() = p.size match {
    case Small => roll3d6() match {
      case p if p <= 8 => (10, 0.42)
      case p if p <= 10 => (15, 0.26)
      case p if p <= 11 => (20, 0.22)
      case p if p <= 12 => (30, 0.19)
      case p if p <= 13 => (40, 0.17)
      case p if p <= 14 => (50, 0.17)
      case p if p <= 15 => (60, 0.17)
      case p if p <= 16 => (70, 0.17)
      case _ => (80, 0.17)
    }
    case Medium => roll3d6() match {
      case p if p <= 8 => (100, 0.18)
      case p if p <= 10 => (150, 0.19)
      case p if p <= 11 => (200, 0.20)
      case p if p <= 12 => (250, 0.22)
      case p if p <= 13 => (300, 0.24)
      case p if p <= 14 => (350, 0.25)
      case p if p <= 15 => (400, 0.26)
      case p if p <= 16 => (450, 0.27)
      case _ => (500, 0.29)
    }
    case Large => roll3d6() match {
      case p if p <= 8 => (600, 0.31)
      case p if p <= 10 => (800, 0.35)
      case p if p <= 11 => (1000, 0.4)
      case p if p <= 12 => (1500, 0.6)
      case p if p <= 13 => (2000, 0.8)
      case p if p <= 14 => (2500, 1.0)
      case p if p <= 15 => (3000, 1.2)
      case p if p <= 16 => (3500, 1.4)
      case _ => (4000, 1.6)
    }
  }

  def determineGasGiantMass() = determineGasGiantMassDensity()._1

  def determineGasGiantDensity() = determineGasGiantMassDensity()._2

  def pressureFactor = {
    if (p.planetType == ChtonianPlanet) 0
    else p.size match {
      case Small => if (p.planetType == IcePlanet) 10 else 0
      case Medium => if (p.planetType == GreenhousePlanet) 100 else 1
      case Large => if (p.planetType == GreenhousePlanet) 500 else 5
      case _ => 0
    }
  }

  def greenhouseFactor = p.planetType match {
    case IcePlanet => p.size match {
      case Small => 0.1
      case Tiny => 0.
      case _ => 0.2
    }
    case AmmoniaPlanet => 0.2
    case OceanPlanet | GardenPlanet => 0.16
    case GreenhousePlanet => 2.0
    case _ => 0.
  }

  def absorptionFactor = p.planetType match {
    case AsteroidBelt => 0.97
    case IcePlanet => p.size match {
      case Tiny => 0.86
      case Small => 0.93
      case _ => 0.86
    }
    case RockPlanet => 0.96
    case HadeanPlanet => 0.67
    case AmmoniaPlanet => 0.84
    case OceanPlanet | GardenPlanet => p.hydrographicCoverage match {
      case c if c <= 0.2 => 0.95
      case c if c <= 0.5 => 0.92
      case c if c <= 0.9 => 0.88
      case _ => 0.84
    }
    case GreenhousePlanet => 0.77
    case ChtonianPlanet => 0.97
    case _ => 0.
  }

  import ClimateType._

  def climateType = surfaceTemperature match {
    case t if t < 244 => Frozen
    case t if t < 255 => VeryCold
    case t if t < 266 => Cold
    case t if t < 278 => Chilly
    case t if t < 289 => Cool
    case t if t < 300 => Normal
    case t if t < 311 => Warm
    case t if t < 322 => Tropical
    case t if t < 333 => Hot
    case t if t < 344 => VeryHot
    case _ => Infernal
  }

  def rotationPeriodModifier = p.planetType match {
    case GasGiant => p.size match {
      case Small => 6
      case _ => 0
    }
    case _ => p.size match {
      case Large => 6
      case Medium => 10
      case Small => 14
      case _ => 18
    }
  }

  def habitabilityScore = {
    import AtmosphericPressure._
    var habitabilityMods = p.atmosphericPressureCategory match {
      case VeryThin => 1
      case Thin => 2
      case Standard | Dense => 3
      case VeryDense | Superdense => 1
      case _ => 0
    }
    if (p.corrosive) habitabilityMods -= 2
    else if (p.toxic != Toxic.NotToxic) habitabilityMods -= 1

    habitabilityMods += (p.hydrographicCoverage match {
      case c if c <= 0.01 => 1
      case c if c <= 0.01 => 0
      case c if c <= 0.59 => 1
      case c if c <= 0.90 => 2
      case c if c <= 0.99 => 1
      case _ => 0
    })

    habitabilityMods += (p.climateType match {
      case Cold => 1
      case Chilly | Cool | Normal | Warm | Tropical => 2
      case Hot => 1
      case _ => 0
    })

    habitabilityMods += ((2 - p.volcanicActivity.id ).min(0) + (2 - p.tectonicActivity.id ).min(0)).max(-2)
    habitabilityMods
  }

  def carryingCapacity = {
    20e6 * ((habitabilityScore + p.resourceValue) match {
      case -5 => 0.03
      case -4 => 0.06
      case -3 => 0.13
      case -2 => 0.25
      case -1 => 0.5
      case 0 => 1.
      case 1 => 2.
      case 2 => 4.
      case 3 => 8.
      case 4 => 15.
      case 5 => 30.
      case 6 => 60.
      case 7 => 130.
      case 8 => 250.
      case 9 => 500.
      case 10 => 100.
      case _ => 0.005
    }) * (p.planetType match {
      case AsteroidBelt => 50.
      case GasGiant => 0
      case _ => math.pow(p.radius / 6378., 2)
    })
  }

  import AtmosphericPressure._

  def atmosphericPressureCategory = atmosphericPressure match {
    case p if p <= 0.01 => Trace
    case p if p <= 0.5 => VeryThin
    case p if p <= 0.8 => Thin
    case p if p <= 1.2 => Standard
    case p if p <= 1.5 => Dense
    case p if p <= 10 => VeryDense
    case _ => Superdense
  }

  def incomeModifier = (habitabilityScore + p.resourceValue) match {
    case 10 => 1.4
    case 9 => 1.2
    case 7 | 8 => 1.0
    case 4 | 5 | 6 => 0.9
    case 1 | 2 | 3 => 0.8
    case _ => 0.7
  }

  def blackbodyCorrection = absorptionFactor * (1 + (p.atmosphericMass * greenhouseFactor))

  def surfaceTemperature = p.blackbodyT * blackbodyCorrection

  def surfaceTemperatureCelsius = surfaceTemperature - 273

  def atmosphericPressure = p.atmosphericMass * pressureFactor * gravity

  def gravity = p.density * p.radius / 6378. // in g
  def mass = p.density * math.pow(p.radius / 6378., 3) // in earth masses
  def dayLength = (p.orbitalPeriod * 365.26 * 24 * p.rotationPeriod) / (p.orbitalPeriod * 365.26 * 24 - p.rotationPeriod) // in hours

}