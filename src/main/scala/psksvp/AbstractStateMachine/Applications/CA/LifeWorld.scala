package psksvp.AbstractStateMachine.Applications.CA

import psksvp.AbstractStateMachine.Core._
/**
  * Created by psksvp on 16/06/2016.
  */

/**
  *
  * @param size
  * @param n
  */
abstract class LifeWorld(size:Int, n:String) extends Rule(n) with Visualizer
{
  protected val world = state[(Int, Int), Int]("world")
  protected val symbol = state[Int, Char]("symbol")

  override def main:Unit = evolve()

  def evolve():Unit

  def countNeighbors(r:Int, c:Int, v:Int = 1):Int=
  {
    var neighbors = 0
    for (i <- -1 to 1; j <- -1 to 1)
    {
      val nr = (r + j + size) % size
      val nc = (c + i + size) % size
      if (world(nr, nc) === v)
        neighbors = neighbors + 1
    }
    neighbors
  }

  def view(states:List[State[_, _]], nthStep:Int):Unit =
  {
    import psksvp.Terminal._
    ANSI.clearScreen
    ANSI.setBackgroundColor(ANSI.black)
    ANSI.setForegroundColor(ANSI.green)
    world.currents.foreach
    {
      case ((r,c), v) => ANSI.setCursor(r, c)
                         print(symbol(v))
    }
    ANSI.setCursor(size, 0)
    print(s"step -> $nthStep")
    Thread.sleep(100)
  }

  override def toString:String =
  {
    val buffer = new Array[Char](size * size)
    world.currents.foreach
    {
      case ((r,c), v) => buffer(r * size + c) = symbol(v)
    }
    buffer.mkString
  }
}


/**
  *
  * @param size
  */
class BrianBrain(size:Int) extends LifeWorld(size, "BrianBrainRule")
{
  final val DEAD = 0
  final val ALIVE = 1
  final val DYING = 2

  override def init:Boolean =
  {
    world := (for(r <- 0 until size;
                  c <- 0 until size) yield ((r,c), psksvp.random(3)))
    symbol(DEAD) := ' '
    symbol(ALIVE) := 'O'
    symbol(DYING) := ':'

    true
  }

  def evolve():Unit=
  {
    world.currents.foreach
    {
      case ((r, c), v) =>
        v match
        {
          case DEAD if 2 == countNeighbors(r, c) => world(r, c) := ALIVE
          case ALIVE                             => world(r, c) := DYING
          case DYING                             => world(r, c) := DEAD
          case _                                 => skip
        }
    }
  }

}

/**
  *
  * @param size
  */
class Conway(size:Int) extends LifeWorld(size, "Conway")
{
  final val DEAD = 0
  final val ALIVE = 1

  override def init:Boolean =
  {
    world := (for(r <- 0 until size;
                  c <- 0 until size) yield ((r,c), psksvp.random(2)))
    symbol(DEAD) := ' '
    symbol(ALIVE) := '+'
    true
  }

  override def evolve():Unit=
  {
    world.currents.foreach
    {
      case ((r, c), v) => val nr = countNeighbors(r, c)
                          v match
                          {
                            case DEAD if 3 == nr            => world(r, c) := ALIVE
                            case ALIVE if nr < 2 || nr >= 4 => world(r, c) := DEAD
                            case _                          => skip
                          }
    }
  }
}

/**
  *
  * @param size
  * @param probSpore
  * @param probSporeToHyphae
  * @param probMushroom
  * @param probSpread
  */
class MushroomFairyRing(size:Int,
                        probSpore:Double = 0.01,
                        probSporeToHyphae:Double = 0.3,
                        probMushroom:Double = 0.9,
                        probSpread:Double = 0.5) extends LifeWorld(size, "MushroomFairyRing")
{
  final val EMPTY = 0
  final val SPORE = 1
  final val YOUNG = 2
  final val MATURING = 3
  final val MUSHROOMS = 4
  final val OLDER = 5
  final val DECLAYING = 6
  final val DEAD1 = 7
  final val DEAD2 = 8
  final val INERT = 9

  override def init:Boolean =
  {
    //embed spore
    world := (for(r <- 0 until size;
                  c <- 0 until size) yield ((r,c), psksvp.random(2)))

    // setup symbol to display
    //symbol := List((EMPTY, ' '), (SPORE, '.'))
    symbol(EMPTY) := ' '
    symbol(SPORE) := '.'
    symbol(YOUNG) := '+'
    symbol(MATURING) := '*'
    symbol(MUSHROOMS) := '#'
    symbol(OLDER) := '^'
    symbol(DECLAYING) := ':'
    symbol(DEAD1) := '-'
    symbol(DEAD2) := '~'
    symbol(INERT) := '_'
    true
  }

  override def evolve():Unit=
  {
    world.currents.foreach
    {
      case ((r, c), v) =>
        v match
        {
          case SPORE     => if(Math.random() < probSporeToHyphae)
                              world(r,c) := YOUNG
          case YOUNG     => world(r, c) := MATURING
          case MATURING  => if(Math.random() < probMushroom)
                              world(r,c) := MUSHROOMS
                            else
                              world(r,c) := OLDER
          case MUSHROOMS => world(r, c) := DECLAYING
          case OLDER     => world(r, c) := DECLAYING
          case DECLAYING => world(r, c) := DEAD1
          case DEAD1     => world(r, c) := DEAD2
          case DEAD2     => world(r, c) := EMPTY
          case EMPTY     => if(Math.random() < probSpread && countNeighbors(r, c, YOUNG) > 0)
                              world(r, c) := YOUNG
          case INERT     => skip
          case _         => skip

        }
    }
  }
}

/**
  *
  * @param size
  * @param probImmune
  * @param probLighting
  */
class ForrestFire(size:Int,
                  probImmune:Double=0.9,
                  probLighting:Double=0.3) extends LifeWorld(size, "ForrestFire")
{
  final val EMPTY = 0
  final val TREE = 1
  final val BURNING = 2

  override def init:Boolean =
  {
    world := (for(r <- 0 until size;
                  c <- 0 until size) yield ((r,c), psksvp.random(3)))
    symbol(EMPTY) := ' '
    symbol(TREE) := '*'
    symbol(BURNING) := '+'
    true
  }

  override def evolve():Unit=
  {
    world.currents.foreach
    {
      case ((r, c), v) =>
        v match
        {
          case TREE    => if(1 == countNeighbors(r, c, BURNING) && Math.random < probImmune)
                            world(r, c) := BURNING
          case BURNING => world(r, c) := EMPTY
          case EMPTY   => skip
          case _       => skip
        }
    }
  }
}


///////////////////////////

object ASMLife
{
  def main(args:Array[String]):Unit=
  {
    val bb = new BrianBrain(60)
    val cc = new Conway(60)
    val mr = new MushroomFairyRing(60)
    val fr = new ForrestFire(60)
    val asm = new Machine(bb, Some(bb))
    asm.run(1000)
    /*
    for(i <- 1 to 10)
    {
      asm.step
      println(mr)
    } */
  }
}

