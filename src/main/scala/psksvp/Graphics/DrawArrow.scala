package psksvp.Graphics

/**
  * Created by psksvp on 27/02/2016.
  */
object DrawArrow
{
  def apply(processingCtx:processing.core.PApplet,
            start:(Float, Float),
            end:(Float, Float),
            size:(Float, Float)=(20f, 15f)): Unit =
  {
    val tailX = start._1
    val tailY = start._2
    val tipX = end._1
    val tipY = end._2

    processingCtx.line(start._1, start._2, end._1, end._2)
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

    processingCtx.beginShape()
    arrow.foreach((p:(Float, Float)) => processingCtx.vertex(p._1, p._2))
    processingCtx.endShape(2)
  }
}
