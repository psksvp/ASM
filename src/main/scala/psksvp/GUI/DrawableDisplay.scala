package psksvp.GUI

import java.io.File
import javax.swing.filechooser.FileNameExtensionFilter

import scala.swing._

/**
 * Created by psksvp on 4/07/2014.
 */
class DrawableDisplay(width:Int, height:Int, caption:String="NoName") extends Drawable
{
  val drawable = new DrawableView
  drawable.setDimension(width, height)

  val zoomInButton = Button("zoom in")
  {
    println("zoom in")
    drawable.increaseZoomFactor(delta=0.5)
  }

  val zoomOutButton = Button("zoom out")
  {
    println("zoom out")
    drawable.decreaseZoomFactor(delta=0.5)
  }

  val saveImageButton = Button("SaveImage")
  {
    val fileChooser = new FileChooser(new File(System.getProperty("user.home")))
    fileChooser.title = "Save Image to file"
    fileChooser.fileSelectionMode = FileChooser.SelectionMode.FilesOnly
    fileChooser.multiSelectionEnabled = false
    fileChooser.fileFilter = new FileNameExtensionFilter("PNG file", "png")

    if(fileChooser.showSaveDialog(null) == FileChooser.Result.Approve)
    {
      drawable.saveImageToFile(fileChooser.selectedFile.getAbsolutePath())
    }
  }

  object zoomControls extends FlowPanel
  {
    contents += zoomInButton
    contents += zoomOutButton
    contents += saveImageButton
  }

  val contentPane = new ScrollPane(drawable)

  private val frameWindow = new Frame
  {
    title = "Graphics::" + caption
    preferredSize = new Dimension(width, height)
    resizable = true
    contents = new BoxPanel(Orientation.Vertical)
    {
      contents += zoomControls
      contents += contentPane //new ScrollPane(drawable)
    }
    visible = true
  }

  override def setDisplayTitleText(caption:String):Unit=
  {
    frameWindow.title = caption
  }

  override def inputResponder=drawable.inputResponder

  override def setInputResponder(responder:InputResponder)
  {
    drawable.setInputResponder(responder)
  }

  override def setDimension(width:Int, height:Int)
  {
    drawable.setDimension(width, height)
  }

  override def dimension():(Int, Int) =
  {
    drawable.dimension()
  }
  /**
   *
   * @return
   */
  override def beginDraw():Graphics2D =
  {
    this.drawable.beginDraw()
  }

  override def endDraw(graphicContext:Graphics2D):Unit =
  {
    this.drawable.endDraw(graphicContext)
  }

  def show():Unit =
  {
    this.frameWindow.pack()
    this.frameWindow.visible = true
  }

  def hide():Unit =
  {
    this.frameWindow.visible = false
  }

}
