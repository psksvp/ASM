package psksvp.Graphics

/**
 * Created by psksvp on 1/01/15.
 */
object ArrowLine
{
  def apply(displayContext:DisplayContext, start:(Float, Float),
                                             end:(Float, Float),
                                             size:(Float, Float)=(20f, 15f)): Unit =
  {
    val tailX = start._1
    val tailY = start._2
    val tipX = end._1
    val tipY = end._2

    displayContext.drawLine(start, end)
    val arrowLength = size._1
    val dx = tipX - tailX
    val dy = tipY - tailY

    val theta = Math.atan2(dy, dx)
    val rad = Math.toRadians(size._2)
    val x = tipX - arrowLength * Math.cos(theta + rad)
    val y = tipY - arrowLength * Math.sin(theta + rad)

    val phi2 = Math.toRadians(-size._2)
    val x2 = tipX - arrowLength * Math.cos(theta + phi2)
    val y2 = tipY - arrowLength * Math.sin(theta + phi2)

    val arrow = new Array[(Float, Float)](3)
    arrow(0) = end
    arrow(1) = (x.toFloat, y.toFloat)
    arrow(2) = (x2.toFloat, y2.toFloat)

    displayContext.drawPolygon(arrow)
  }
}
