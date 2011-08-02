package timer

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 7/1/11
 * Time: 11:55 AM
 * To change this template use File | Settings | File Templates.
 */

class Timer {
  private var startTime = 0l

  def start() = startTime = System.currentTimeMillis()
  def lap() = {
    val res = System.currentTimeMillis() - startTime
    startTime = System.currentTimeMillis()
    res / 1000.
  }

}