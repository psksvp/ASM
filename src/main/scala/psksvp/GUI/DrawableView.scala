package psksvp.GUI

import java.awt.image.BufferedImage
import java.awt.{Dimension, Graphics2D}

import scala.swing.Component
import scala.swing.event.{KeyTyped, MouseClicked, MouseDragged}

/**
 * Created by psksvp on 4/07/2014.
 */
class DrawableView extends Component with Drawable
{
  private var width:Int=0
  private var height:Int=0
  private var zoomFactor:Double = 1.0
  private var drawing:Boolean = false
  private var offScreenImage:BufferedImage = null
  private var onScreenImage:BufferedImage = null


  listenTo(mouse.clicks, mouse.moves, keys)
  reactions +=
  {
    case MouseClicked(_, p, _, _, _) =>
    {
      if(null != this.inputResponder)
        this.inputResponder.mouseClicked((p.x.toDouble / zoomFactor).toInt, (p.y.toDouble / zoomFactor).toInt)
    }
    case MouseDragged(_, p, _) =>
    {
      if(null != this.inputResponder)
        this.inputResponder.mouseClicked((p.x.toDouble / zoomFactor).toInt, (p.y.toDouble / zoomFactor).toInt)
    }
    case KeyTyped(_, c, _, _) =>
    {
      if(null != this.inputResponder)
        this.inputResponder.characterTyped(c)
    }
  }

  def setZoomFactor(z:Double):Unit=
  {
    this.zoomFactor = z
    this.preferredSize = new Dimension((width.toDouble*zoomFactor).toInt, (height.toDouble*zoomFactor).toInt)
    this.repaint()
  }

  def increaseZoomFactor(delta:Double):Unit=
  {
    this.setZoomFactor(this.zoomFactor + delta)
  }

  def decreaseZoomFactor(delta:Double):Unit=
  {
    if(this.zoomFactor - delta <= 1.0)
      this.setZoomFactor(1.0)
    else
      this.setZoomFactor(this.zoomFactor - delta)
  }

  def saveImageToFile(filename:String):Boolean=
  {
    this.synchronized
    {
      if(null != this.onScreenImage)
      {
        javax.imageio.ImageIO.write(this.onScreenImage, "png", new java.io.File(filename + ".png"))
        true
      }
      else
      {
        println("GraphicsView::saveImageToFile : fail to write file " + filename + ".png")
        false
      }
    }
  }

  override def paintComponent(g:Graphics2D)
  {
    this.synchronized
    {
      if(false == drawing && null != this.onScreenImage)
      {
        //g.setRenderingHint(KEY_ANTIALIASING, VALUE_ANTIALIAS_ON)

        //g.drawImage(onScreenImage.getScaledInstance(this.width, this.height, Image.SCALE_SMOOTH), 0, 0, null)
        g.scale(zoomFactor, zoomFactor)
        g.drawImage(onScreenImage, 0, 0, null)
      }
    }
  }

  override def setDimension(width:Int, height:Int)
  {
    this.synchronized
    {
      this.width = width
      this.height = height
      this.offScreenImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      this.onScreenImage = new BufferedImage(width, height, BufferedImage.TYPE_4BYTE_ABGR)
      this.preferredSize = new Dimension((width.toDouble*zoomFactor).toInt, (height.toDouble*zoomFactor).toInt)
      this.visible = true
      this.focusable = true
    }
  }

  override def dimension():(Int, Int) =
  {
    (this.width, this.height)
  }

  override def beginDraw():Graphics2D =
  {
    require(this.width > 0 && this.height > 0, "GraphicsView::setDimension has not been called once")
    this.drawing = true
    offScreenImage.createGraphics()
  }

  override def endDraw(graphicContext:Graphics2D):Unit =
  {
    this.synchronized
    {
      val g = onScreenImage.createGraphics()
      g.drawImage(offScreenImage, 0, 0, null)
      g.dispose()
      graphicContext.dispose()
      this.drawing = false
      this.repaint()
    }
  }
}
