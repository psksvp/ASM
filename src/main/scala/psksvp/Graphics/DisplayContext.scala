package psksvp.Graphics

import scala.collection.mutable.ListBuffer

/**
 * Created by psksvp on 19/12/14.
 */
trait DisplayContext
{
  private val responderList = new ListBuffer[DisplayResponder]()

  def addResponder(d:DisplayResponder): Unit =
  {
    responderList.append(d)
  }

  def removeResponder(d:DisplayResponder): Unit =
  {
    val index = responderList.indexOf(d)
    if(index >= 0)
      responderList.remove(index)
  }

  def responders:Seq[DisplayResponder]=responderList

  def drawPixel(coord:(Float, Float))
  def drawLine(start:(Float, Float), end:(Float,Float))
  def drawRectangle(topLeft:(Float, Float), size:(Float, Float))
  def drawEllipse(center:(Float, Float), size:(Float, Float))
  def drawPolygon(listOfVertex:Seq[(Float,Float)])
  def drawText(coord:(Float, Float), text:String)
  def drawArrayOfARGB(buffer:Array[Int], width:Int, height:Int, coord:(Float, Float))
  def eraseBackground(color:(Float, Float, Float, Float) = (255f, 255f, 255f, 255f))

  def setFillColor(color:(Float, Float, Float, Float))
  def setStrokeColor(color:(Float, Float, Float, Float))
  def setStrokeWight(weight:Float)
  def setTextSize(size:Float)

  def redraw()
  def setAutoRedraw(auto:Boolean)

  def width:Int
  def height:Int
  def center:(Float, Float)=(width/2f, height/2f)

  def drawLines(coords:Seq[(Float, Float)]):Unit=
  {
    if(coords.length > 1)
    {
      var s = coords.head
      for(v <- coords.tail)
      {
        drawLine(s, v)
        s = v
      }
    }
  }


  // new
  def drawShape(s:Shape)
}


