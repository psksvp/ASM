package psksvp

/**
  * Created by psksvp on 17/01/2016.
  */
package object Graphics
{
  implicit def tupleToCoordinate2D(c:(Float, Float)):Coordinate=Coordinate(c._1, c._2)
  implicit def tupleToSize2D(s:(Float, Float)):Size=Size(s._1, s._2)
}
