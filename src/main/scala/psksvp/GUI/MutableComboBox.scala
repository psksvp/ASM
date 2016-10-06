package psksvp.GUI

/**
 * Created by psksvp on 23/07/2014.
 */
import scala.swing.{ Component, Swing, event }
import javax.swing.JComboBox

/**
 * Very basic Mutable ComboBox for Scala.
 * <p>Sample usage:<p>
 * <pre>
 * val box = new MutableComboBox[String]
 * box.items = List("1", "11", "222")
 * listenTo(box)
 * reactions += {
 *   case SelectionChanged(`box`) => println(box.item)
 * }
 * </pre>
 * <p>Note that there is no separate "selection" member. This combobox publishes event on its own</p>
 */
class MutableComboBox[T] extends Component
{
  override lazy val peer = new JComboBox[T]() with SuperMixin

  peer.addActionListener(Swing.ActionListener { e =>
    publish(event.SelectionChanged(MutableComboBox.this))
  })

  def clear=peer.removeAllItems()

  def items_=(s: Seq[T])
  {
    peer.removeAllItems
    s.map(peer.addItem)
  }

  def items = (0 until peer.getItemCount()).map(peer.getItemAt)

  def selectedIndex: Int = peer.getSelectedIndex
  def selectedIndex_=(n: Int) { peer.setSelectedIndex(n) }
  def selectedItem: T = peer.getSelectedItem.asInstanceOf[T]
  def selectedItem_=(a: T) { peer.setSelectedItem(a) }

}
