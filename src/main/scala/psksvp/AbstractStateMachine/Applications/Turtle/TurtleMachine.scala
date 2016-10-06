package psksvp.AbstractStateMachine.Applications.Turtle

/**
  * Created by psksvp on 4/10/16.
  */
import psksvp.AbstractStateMachine.Core._
import psksvp.AbstractStateMachine.Applications.Turtle.TurtleMachine._
import psksvp.{pop, push, top}

/**
  *
  * @param instructions
  */
class TurtleMachine(instructions:List[Instruction]) extends Rule("Turtle")
{
  type Stream = Set[(Int, Instruction)]
  type Coord2D = (Float, Float)
  type Frame = (Boolean, Float, Coord2D, Int, Stream)

  private val pen = state[Boolean]("PEN")
  private val heading = state[Float]("HEADING")
  private val position = state[Coord2D]("POSITION")
  private val pc = state[Int]("PC")
  private val stream = state[Int, Instruction]("STREAM")
  private val stack = state[List[Frame]]("STACK")
  private val halt = state[Boolean]("HALT")

  def currentFrame:Frame = (pen, heading, position, pc, stream)

  override def init:Boolean =
  {
    pen := true
    heading := 90f
    position := (200f, 200f)
    pc := 0
    stack := Nil
    //println(sanitize(instructions) :+ Halt())
    stream := toStream(sanitize(instructions) :+ Halt())
    halt := false
    true
  }

  override def main:Unit =
  {
    if(!halt)
    {
      val inst = stream(pc)
      execute(inst)
      println(s"executed instruction $inst")
    }
    else
      println("Turtle machine halted")
  }

  private def execute(instruction:Instruction):Unit= instruction match
  {
    case Forward(d)  => val theta = Math.toRadians(heading.toDouble)
                        val r = (d * Math.cos(theta).toFloat,
                                 d * Math.sin(theta).toFloat * -1f)
                        position := (position._1 + r._1, position._2 + r._2)
                        pc := pc + 1

    case Backward(d) => val b = Branch(Turn(heading - 180) :: Forward(d) :: Turn(heading) :: Nil)
                        execute(b)
                        pc := pc + 1

    case Move(x, y)  => position := (x, y)
                        pc := pc + 1

    case Turn(d)     => heading := d
                        pc := pc + 1

    case Right(d)    => heading := heading - d
                        pc := pc + 1

    case Left(d)     => heading := heading + d
                        pc := pc + 1

    case PenUp()     => pen := false
                        pc := pc + 1

    case PenDown()   => pen := true
                        pc := pc + 1

    case Branch(ls)  => stack := push[Frame](stack, (pen, heading, position, pc, stream))
                        pc := 0
                        stream := toStream(ls :+ Return())

    case Return()    => val (pen_, heading_, position_, pc_, stream_) = top(stack)
                        pen := pen_
                        heading := heading_
                        position := position_
                        pc := pc_ + 1
                        stream := stream_
                        stack := pop(stack)

    case NoOP()      => pc := pc + 1

    case Halt()      => halt := true
  }
}


/**
  *
  */
object TurtleMachine
{
  abstract class Instruction

  case class Branch(ls:List[Instruction]) extends Instruction
  case class Return() extends Instruction
  case class Forward(distance:Float) extends Instruction
  case class Backward(distance:Float) extends Instruction
  case class Move(x:Float, y:Float) extends Instruction
  case class Turn(degree:Float) extends Instruction
  case class Left(degree:Float) extends Instruction
  case class Right(degree:Float) extends Instruction
  case class PenUp() extends Instruction
  case class PenDown() extends Instruction
  case class NoOP() extends Instruction
  case class Halt() extends Instruction

  def sanitize(ls:List[Instruction]):List[Instruction] = ls match
  {
    case Nil               => Nil
    case Branch(l) :: rest => if(Nil == l) sanitize(rest)
                              else Branch(sanitize(l)) :: sanitize(rest)
    case h :: rest         => h :: sanitize(rest)
  }

  def toStream(ls:List[Instruction]):Set[(Int, Instruction)] =
  {
    (for(i <- 0 until ls.length) yield((i, ls(i)))).toSet
  }
}

/**
  *
  * @param tm
  */
class Display(tm:TurtleMachine) extends Visualizer
{
  import psksvp.GUI.DrawableDisplay
  import java.awt.Color

  val ctx = new DrawableDisplay(600, 400, "Turtle World")
  var p1:Option[(Float, Float)] = None

  override def view(states:List[State[_, _]], nthStep:Int):Unit =
  {
    val g = ctx.beginDraw()
    g.setColor(Color.BLACK)
    val (_, _, p2, _, _) = tm.currentFrame
    p1 match
    {
      case Some((x, y)) => g.drawLine(x.toInt, y.toInt, p2._1.toInt, p2._2.toInt)
      case _            =>
    }
    p1 = Some(p2)
    ctx.endDraw(g)
  }
}

//class Display2(tm:TurtleMachine) extends Visualizer
//{
//  import psksvp.Graphics.ProcessingAppletDisplay
//  val display = new ProcessingAppletDisplay(400, 400)
//  var p1:Option[(Float, Float)] = None
//
//  override def view(states:List[State[_, _]], nthStep:Int):Unit =
//  {
//    val (_, _, p2, _, _) = tm.currentFrame
//    p1 match
//    {
//      case Some(coord) => display.drawLine(coord, p2)
//      case _           =>
//    }
//    p1 = Some(p2)
//  }
//}

/**
  *
  */
object TurtleProgram
{
  def main(args:Array[String]):Unit=
  {
    val instructions = Move(10, 300) :: spiral(200)
    val rule = new TurtleMachine(instructions)
    val display = new Display(rule)
    val machine = new Machine(rule, Some(display))
    machine.run()
  }


  def spiral(len:Int):List[Instruction]=
  {
    if(len > 0)
      Forward(len)::Right(60)::Branch(spiral(len - 2))::Nil
    else
      Nil
  }

}


