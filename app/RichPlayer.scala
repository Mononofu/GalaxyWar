package RichPlayer

/**
 * Created by IntelliJ IDEA.
 * User: mononofu
 * Date: 7/25/11
 * Time: 10:59 PM
 * To change this template use File | Settings | File Templates.
 */

import BCrypt.BCrypt

case class RichPlayer(name: String, login: String, password: String) {
  def checkPassword(rawPassword: String) = BCrypt.checkpw(rawPassword, password)
}