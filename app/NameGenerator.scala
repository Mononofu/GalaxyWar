package NameGenerator

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 7/23/11
 * Time: 12:06 AM
 * To change this template use File | Settings | File Templates.
 */

import java.io.{ObjectInputStream, FileInputStream}

class NameGenerator {

  val rnd = new scala.util.Random()
  // key: two previous chars
  // value: list of next chars with probabilities
  var probs = scala.collection.mutable.Map[(Char, Char), List[(Double, Char)]]()

  def load(filename: String) {
    val fis = new FileInputStream(filename)
    val ois = new java.io.ObjectInputStream(fis)

    probs = ois.readObject().asInstanceOf[scala.collection.mutable.Map[(Char, Char), List[(Double, Char)]]]
  }

  def getChar(chars: List[(Double, Char)], draw: Double): Char = {
    var total = 0.
    for ( (prob, c) <- chars) {
      if (draw >= total && draw <= (total + prob))
        return c
      total += prob
    }
    ' '
  }

  def generateWord(): String = {
    var choices = List[Char]()
    var pprev, prev = ' '
    var c = getChar(probs((pprev, prev)), rnd.nextDouble())
    while (c != ' ') {
      choices ::= c
      pprev = prev
      prev = c
      c = getChar(probs((pprev, prev)), rnd.nextDouble())
    }
    choices.reverse.mkString
  }

  def generateWord(minLen: Int, maxLen: Int): String = {
    var word = generateWord()
    while ( !(minLen <= word.length() && word.length() <= maxLen)) {
      word = generateWord()
    }
    word
  }
}
