package psksvp.Graphics

/**
  * Created by psksvp on 19/11/2015.
  */
abstract class MouseButton
case class LeftButton() extends MouseButton
case class RightButton() extends MouseButton

abstract class InputAction
abstract class MouseAction(x:Float, y:Float) extends InputAction
case class MousePressed(x:Float, y:Float, button:MouseButton) extends MouseAction(x, y)
case class MouseReleased(x:Float, y:Float, button:MouseButton) extends MouseAction(x, y)
case class MouseMoved(x:Float, y:Float) extends MouseAction(x, y)
case class MouseDragged(x:Float, y:Float) extends MouseAction(x, y)

case class KeyPressed(keyCode:Int, char:Char) extends InputAction
case class KeyReleased(keyCode:Int, char:Char) extends InputAction