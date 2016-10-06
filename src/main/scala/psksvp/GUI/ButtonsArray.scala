package psksvp.GUI



import scala.swing.{Button, Orientation, BoxPanel}

/**
 * Created by psksvp on 2/08/2014.
 */
class ButtonsArray(buttonsText:Seq[String],
                   orientation:Orientation.Value=Orientation.Vertical) extends BoxPanel(orientation)
{
  for(title <- buttonsText)
  {
    contents += new Button(title)
  }
}
