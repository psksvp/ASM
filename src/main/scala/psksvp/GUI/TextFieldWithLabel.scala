package psksvp.GUI

import scala.swing._

/**
 * Created by psksvp on 19/07/2014.
 */
class TextFieldWithLabel(label:String, initText:String="", nColumn:Int=3) extends FlowPanel
{
  private val textField = new TextField()
  textField.text = initText
  textField.columns = nColumn

  contents += new Label(label)
  contents += textField


  def text=textField.text
}
