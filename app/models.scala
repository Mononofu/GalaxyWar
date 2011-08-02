package models

import org.squeryl.PrimitiveTypeMode._
import org.squeryl.Schema
import org.squeryl.annotations.Column

case class Empire(
  id: Long,
  fullname: String,
  description: String,
  player_id: Option[Long]
  ) {
  def this() = this (0, "unnamed", "", Some(0l))
}

case class StarSystem(
  id: Long,
  pos_x: Long, // all in km
  pos_y: Long,
  pos_z: Long,
  fullname: String = "unnamed"
  )

case class Star(
  id: Long,
  starsystem_id: Option[Long],
  mass: Double, // in sun masses
  age: Long = 0, // in years
  orbitRadius: Long = 0, // in km
  orbitEccentricity: Double = 0.0, // from 0 to 2
  orbitalPeriod: Double = 0.0, // in earth years
  fullname: String = "unnamed"
  ) {
  def this() = this (0, Some(0l), 0.0)
}


import RichPlanet.PlanetType._
import RichPlanet.Size._
import RichPlanet.Toxic._
import RichPlanet.MarginalAtmosphere._
import RichPlanet.VolcanicActivity._

case class Planet(
  id: Long,
  star_id: Option[Long],
  orbitRadius: Double, // in AU
  var planetType: PlanetType,
  var size: Size,
  var blackbodyT: Int = 0,
  var atmosphericMass: Double = 0,
  var suffocating: Boolean = false,
  var corrosive: Boolean = false,
  var toxic: Toxic = NotToxic,
  var marginalAtmosphere: MarginalAtmosphere = NotMarginal,
  var hydrographicCoverage: Double = 0.,
  var radius: Int = 0,
  var density: Double = 0.0,
  var rings: Int = 0, // number of rings the planet has
  var moonlets: Int = 0, // real moons have their own table // abused to store orbit radius of moons
  var orbitalPeriod: Double = 0., // in earth years
  var rotationPeriod: Double = 0., // in hours
  var axialTilt: Int = 0, // in degrees
  var volcanicActivity: VolcanicActivity = NoActivity,
  var tectonicActivity: VolcanicActivity = NoActivity,
  var resourceValue: Int = 0
  ) {
  def this() = this (0, Some(0l), 0l, Empty, Medium)
}

// large moon
case class Moon( // a moon is basically a planet
  id: Long,
  parent_planet_id: Option[Long],  // so store the planet we belong to
  desc_planet_id: Option[Long]  // and the planet which describes the moon
  // just set it's star id to None
  ) {
  def this() = this(0l, Some(0l), Some(0l))
}

case class Player (
  id: Long,
  name: String,
  login: String,
  password: String
  )

case class Colony (
  id: Long,
  planet_id: Option[Long],    // on which planet is the colony
  player_id: Option[Long]     // which player does the colony belong to?
  )


// allow players to save planets of interest to them
case class SavedPlanet (
  id: Long,
  planet_id: Option[Long],
  player_id: Option[Long]
  )
  
object Game extends Schema {
  val empires     = table[Empire]
  val starsystems = table[StarSystem]
  val stars       = table[Star]
  val planets     = table[Planet]
  val moons       = table[Moon]
  val players     = table[Player]

  on(starsystems)(s => declare(
    s.id  is(primaryKey, indexed)
  ))

  on(stars)(s => declare(
    s.id            is(primaryKey),
    s.starsystem_id is(indexed)
  ))

  on(planets)(p => declare(
    p.id      is(primaryKey, indexed),
    p.star_id is(indexed)
  ))

  on(moons)(m => declare(
    m.id                is(primaryKey),
    m.parent_planet_id  is(indexed)
  ))
}