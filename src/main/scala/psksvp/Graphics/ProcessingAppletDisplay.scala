package psksvp.Graphics

/**
 * Created by psksvp on 10/12/14.
 */

class ProcessingAppletDisplay(displayWidth:Int,
                              displayHeight:Int,
                              title:String="ProcessingDisplay",
                              resizable:Boolean=false) extends DisplayContext
{
  private val applet = new Applet(this)

  show()

  class Applet(context: ProcessingAppletDisplay) extends processing.core.PApplet
  {
    override def settings:Unit=super.size(context.width, context.height)
    override def draw:Unit= responders.foreach(_.draw(context))
    override def mouseMoved:Unit=responders.foreach(_.input(context, MouseMoved(mouseX, mouseY)))
    override def mouseDragged:Unit=responders.foreach(_.input(context, MouseDragged(mouseX, mouseY)))
    override def mousePressed:Unit=
    {
      val button = if(0 == mouseButton) LeftButton() else RightButton()
      responders.foreach(_.input(context, MousePressed(mouseX, mouseY, button)))
    }
    override def mouseReleased:Unit=
    {
      val button = if(0 == mouseButton) LeftButton() else RightButton()
      responders.foreach(_.input(context, MouseReleased(mouseX, mouseY, button)))
    }

    override def keyPressed:Unit=responders.foreach(_.input(context, KeyPressed(keyCode, key)))
    override def keyReleased:Unit=responders.foreach(_.input(context, KeyReleased(keyCode, key)))
  }

  def show(): Unit =
  {
    processing.core.PApplet.runSketch(Array(""), applet)
  }



  /////////////////////////////////////////////////////////////////////////
  override def drawPixel(coord:(Float, Float))
  {
    applet.point(coord._1, coord._2)
  }

  override def drawLine(start: (Float, Float), end: (Float, Float)): Unit =
  {
    applet.line(start._1, start._2, end._1, end._2)
  }

  override def drawRectangle(topLeft: (Float, Float), size: (Float, Float)): Unit =
  {
    applet.rect(topLeft._1, topLeft._2, size._1, size._2)
  }

  override def drawEllipse(center: (Float, Float), size: (Float, Float)): Unit =
  {
    applet.ellipse(center._1, center._2, size._1, size._2)
  }

  override def drawPolygon(listOfVertex: Seq[(Float, Float)]): Unit =
  {
    applet.beginShape()
    listOfVertex.foreach((p:(Float, Float)) => applet.vertex(p._1, p._2))
    applet.endShape(2)
  }


  override def setStrokeWight(weight: Float): Unit =
  {
    applet.strokeWeight(weight)
  }

  override def setFillColor(color: (Float, Float, Float, Float)): Unit =
  {
    applet.fill(color._1, color._2, color._3, color._4)
  }

  override def setStrokeColor(color: (Float, Float, Float, Float)): Unit =
  {
    applet.stroke(color._1, color._2, color._3, color._4)
  }

  override def eraseBackground(color: (Float, Float, Float, Float)): Unit =
  {
    applet.background(color._1, color._2, color._3, color._4)
  }

  override def drawArrayOfARGB(buffer:Array[Int], width:Int, height:Int, coord:(Float, Float)): Unit =
  {
    val pimage = applet.createImage(width, height, 2) // 2 -> ARGB, bad bad
    var i = 0;
    while(i < width * height)
    {
      pimage.pixels(i) = buffer(i)
      i += 1
    }

    applet.image(pimage, coord._1, coord._2)
  }

  override def setTextSize(size:Float):Unit=
  {
    applet.textSize(size)
  }

  override def drawText(coord:(Float, Float), text:String):Unit=
  {
    applet.text(text, coord._1, coord._2)
  }

  override def redraw(): Unit =
  {
    applet.redraw()
  }

  override def setAutoRedraw(auto:Boolean):Unit=
  {
    if(true == auto)
      applet.loop()
    else
      applet.noLoop()
  }

  def width:Int=this.displayWidth
  def height:Int=this.displayHeight


  def drawShape(s:Shape):Unit=
  {
    applet.beginShape()
    s.vertexList.foreach((p:Coordinate) => applet.vertex(p.x, p.y))
    applet.endShape(2)
  }
}
