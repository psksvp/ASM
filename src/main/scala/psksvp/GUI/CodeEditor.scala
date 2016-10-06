package psksvp.GUI

import javax.swing.event.{DocumentEvent, DocumentListener}
import org.fife.ui.rtextarea._
import org.fife.ui.rsyntaxtextarea._

import scala.swing._

/**
 * Created by psksvp on 25/07/2014.
 */
class CodeEditor(language:String=SyntaxConstants.SYNTAX_STYLE_SCALA) extends BoxPanel(Orientation.Vertical)
                                                                     with DocumentListener
{
  val editor = new RSyntaxTextArea()
  editor.setSyntaxEditingStyle(language)
  editor.setCodeFoldingEnabled(true)
  editor.setAntiAliasingEnabled(true)
  editor.getDocument().addDocumentListener(this)
  val scrollPane = new RTextScrollPane(editor)

  contents += Component.wrap(scrollPane)

  def text=editor.getText()
  def setText(text:String)=editor.setText(text)
  def setCursorAtLine(number:Int):Unit=
  {
    require(number >= 1, "psksvp.GUI.CodeEditor.setCursorAtLine, line number in parameter must be >= 1")
    val n = editor.getDocument().getDefaultRootElement().getElement(number - 1).getStartOffset()
    editor.setCaretPosition(n)
  }

  private var _modified:Boolean = false
  def modified=_modified
  def modified_=(value:Boolean):Unit = _modified = value
  def makeClean():Unit=
  {
    modified = false
  }

  override def changedUpdate(e:DocumentEvent):Unit=
  {
    modified = true
  }

  override def insertUpdate(e:DocumentEvent):Unit=
  {
    modified = true
  }

  override def removeUpdate(e:DocumentEvent):Unit=
  {
    modified = true
  }
}
