package psksvp.Graphics

/**
  * Created by psksvp on 24/12/2015.
  */

case class Coordinate(x:Float, y:Float, z:Float=0)
case class Size(width:Float, height:Float, Depth:Float=0)


abstract class Shape
{
  def vertexList:Seq[Coordinate]
}

case class Rectangle(topLeft:Coordinate, size:Size) extends Shape
{
  def vertexList:Seq[Coordinate]=List(topLeft,
                                      Coordinate(topLeft.x + size.width, topLeft.y),
                                      Coordinate(topLeft.x + size.width, topLeft.y + size.height),
                                      Coordinate(topLeft.x, topLeft.y + size.height))
}

case class Line(start:Coordinate, end:Coordinate) extends Shape
{
  def vertexList:Seq[Coordinate]=List(start, end)
}

case class ArrowHeadLine(start:Coordinate, end:Coordinate, size:Size) extends Shape
{
  val vertices = scala.collection.mutable.ListBuffer[Coordinate]()
  vertices.append(start)
  vertices.append(end)

  val tailX = start.x
  val tailY = start.y
  val tipX = end.x
  val tipY = end.y
  val arrowLength = size.width
  val dx = tipX - tailX
  val dy = tipY - tailY
  val theta = Math.atan2(dy, dx)
  val rad = Math.toRadians(size.height)
  val x = tipX - arrowLength * Math.cos(theta + rad)
  val y = tipY - arrowLength * Math.sin(theta + rad)
  val phi2 = Math.toRadians(-size.height)
  val x2 = tipX - arrowLength * Math.cos(theta + phi2)
  val y2 = tipY - arrowLength * Math.sin(theta + phi2)
  vertices.append(Coordinate(x.toFloat, y.toFloat))
  vertices.append(Coordinate(x2.toFloat, y2.toFloat))

  def vertexList=vertices.toSeq
}

case class Ellipse(center:Coordinate, size:Size, step:Float=50) extends Shape
{
  val sx = size.width / 2
  val sy = size.height / 2
  val twoPI = (2 * Math.PI).toFloat
  val vertices = scala.collection.mutable.ListBuffer[Coordinate]()
  val theta = twoPI / step
  var a = 0f
  while(a < twoPI)
  {
    val coord = Coordinate(center.x + sx * Math.cos(a).toFloat,
                           center.y + sy * Math.sin(a).toFloat)

    vertices.append(coord)
    a = a + theta
  }

  def vertexList=vertices.toSeq
}

case class Circle(center:Coordinate, radius:Float, step:Float=50) extends Shape
{
  def vertexList=Ellipse(center, Size(radius * 2, radius * 2), step).vertexList
}


