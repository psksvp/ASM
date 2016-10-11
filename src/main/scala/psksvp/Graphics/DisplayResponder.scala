/**
  * Created by psksvp on 21/12/14.
  */
package psksvp.Graphics

trait DisplayResponder
{
  def draw(displayContext: DisplayContext): Unit =
  {
  }

  def input(context:DisplayContext, action:InputAction):Unit=
  {
  }

  def resized(newSize:(Float, Float)): Unit =
  {
  }
}