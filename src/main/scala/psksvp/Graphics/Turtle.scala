//   _____           _   _         ____                 _     _
//  |_   _|   _ _ __| |_| | ___   / ___|_ __ __ _ _ __ | |__ (_) ___ ___
//    | || | | | '__| __| |/ _ \ | |  _| '__/ _` | '_ \| '_ \| |/ __/ __|
//    | || |_| | |  | |_| |  __/ | |_| | | | (_| | |_) | | | | | (__\__ \
//    |_| \__,_|_|   \__|_|\___|  \____|_|  \__,_| .__/|_| |_|_|\___|___/
//                                               |_|

package psksvp.Graphics

import scala.collection.mutable.ListBuffer
/**
 * Created by psksvp on 4/08/2014.
 */
class Turtle(displayContext: DisplayContext) extends DisplayResponder
{
  private var delay = 0
  private var batch = false
  private var needRedraw = true
  private var drawing = true
  private var heading = 90f
  private val homePosition = (displayContext.width/2f, displayContext.height/2f)
  private var currentPosition = homePosition
  private val vertexList = new ListBuffer[((Float, Float), (Float, Float))]()

  displayContext.addResponder(this)

  override def draw(displayContext: DisplayContext): Unit =
  {
    if(true == needRedraw)
    {
      vertexList.synchronized
      {
        displayContext.eraseBackground()
        for (v <- vertexList)
          displayContext.drawLine(start = v._1, end = v._2)
        needRedraw = false
        drawTurtle()
        //println("draw")
      }
    }
  }

  private def drawTurtle(size:(Float, Float)=(20f, 15f)): Unit =
  {
    val theta = Math.toRadians(heading)
    val r = (size._1 * Math.cos(theta).toFloat,
             size._1 * Math.sin(theta).toFloat * -1f)
    val endCoord = (currentPosition._1 + r._1, currentPosition._2 + r._2)
    ArrowLine(displayContext, start=currentPosition, end=endCoord)
  }

  private def setNeedRedraw: Unit =
  {
    if(false == batch)
      needRedraw = true
  }

  def forward(distance:Float):Unit=
  {
    val theta = Math.toRadians(heading)
    val r = (distance * Math.cos(theta).toFloat,
             distance * Math.sin(theta).toFloat * -1f)   // y increases downward

    val endCoord = (currentPosition._1 + r._1, currentPosition._2 + r._2)
    if(true == drawing)
    {
      vertexList.synchronized
      {
        vertexList.append((currentPosition, endCoord))
        setNeedRedraw
      }
    }

    currentPosition = endCoord
    if(0 != delay)
      Thread.sleep(delay.toLong)
  }

  def backward(distance:Float):Unit=
  {
    val tempHeading = heading
    heading -= 180
    forward(distance)
    heading = tempHeading
  }

  def left(degree:Float):Unit=
  {
    this.heading += degree
    setNeedRedraw
  }

  def right(degree:Float):Unit=
  {
    this.heading -= degree
    setNeedRedraw
  }

  def setHeading(degree:Float): Unit =
  {
    this.heading = degree
    setNeedRedraw
  }

  def penUp:Unit = this.drawing = false
  def penDown:Unit = this.drawing = true
  def setPenColor(color:(Float, Float, Float, Float)): Unit = this.displayContext.setStrokeColor(color)

  def setPenSize(size:Float):Unit = this.displayContext.setStrokeWight(size)

  def home: Unit =
  {
    this.currentPosition = this.homePosition
    setNeedRedraw
  }

  def clear: Unit =
  {
    vertexList.clear()
    setNeedRedraw
  }

  def beginBatch: Unit = this.batch = true

  def endBatch: Unit =
  {
    this.batch = false
    this.needRedraw = true
  }

  def setDelay(duration:Int): Unit = delay = duration
}


