package psksvp.GUI

import java.awt.Graphics2D

/**
 * Created by psksvp on 4/07/2014.
 */
trait Drawable
{
  private var mInputResponder:InputResponder = null

  def beginDraw():Graphics2D
  def endDraw(graphicContext:Graphics2D):Unit

  def dimension():(Int, Int)
  def setDimension(width:Int, height:Int)

  def setDisplayTitleText(caption:String)
  {
  }

  def inputResponder = this.mInputResponder
  def setInputResponder(responder:InputResponder)
  {
    this.mInputResponder = responder
  }
}
