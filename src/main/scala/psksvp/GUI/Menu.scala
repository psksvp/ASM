package psksvp.GUI

import java.awt.Toolkit
import java.awt.event.ActionListener
import javax.swing.{KeyStroke, JMenuItem}

/**
 * Created by psksvp on 29/07/2014.
 */
class Menu(name:String, listener:ActionListener) extends scala.swing.Menu(title0=name)
{
  def addItem(itemName:String, accelerator:Int = -1):Unit=
  {
    val mi = new JMenuItem(itemName)
    if(-1 != accelerator)
      mi.setAccelerator(KeyStroke.getKeyStroke(accelerator, Toolkit.getDefaultToolkit().getMenuShortcutKeyMask()))
    mi.addActionListener(listener)
    peer.add(mi)
  }

  def addSeparator:Unit=
  {
    peer.addSeparator()
  }
}
