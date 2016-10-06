package psksvp.GUI

/**
 * Created by psksvp on 18/07/2014.
 */
trait InputResponder
{
  def mouseClicked(x:Int, y:Int):Unit
  def characterTyped(c:Char):Unit
}
