package psksvp.AbstractStateMachine.Core

/**
  * Created by psksvp on 1/06/2016.
  */



/**
  *
  * @param name
  * @param stackSize
  * @tparam L
  * @tparam V
  */
class State[L, V](val name:String, stackSize:Int = 2) //extends com.typesafe.scalalogging.LazyLogging
{
  import scala.collection.mutable.HashMap

  /**
    *
    * @param location
    */
  class Mutator(location:L)
  {
    def get:V = memory(location)
    def set(value:V):Unit = setUpdate(location, value)

    def := (value:V):Unit = set(value)
    def := (m:Mutator):Unit = set(m.get)
    def === (other:State[L, V]#Mutator) = this.get == other.get
    def =!= (other:State[L, V]#Mutator) = this.get != other.get
    def === (other:V) = this.get == other
    def =!= (other:V) = this.get != other

    override def toString = memory(location).toString
  }

  private val memory = HashMap[L, V]()
  private val updates = HashMap[L, V]()

  private[Core] def commit:Unit=
  {
    updates.foreach
    {
      case (l, v) => memory(l) = v
    }

    updates.clear()
  }

  private[Core] def isFixedPoint:Boolean=
  {
    var fixed = true
    updates.foreach
    {
      case (l, v) => if(v != memory(l))
                       fixed = false
    }

    fixed
  }

  def apply(location:L):Mutator = new Mutator(location)

  def setUpdate(location:L, value:V):Unit =
  {
    if(updates.isDefinedAt(location) && updates(location) != value)
      sys.error(s"state $name:error -> trying to update the same location($location) twice with different value")
    else
    {
      //if(updates.isDefinedAt(location) && updates(location) == value)
      //  println(s"state $name: updating at $location to the same value")
      updates(location) = value
    }
  }

  def locations = memory.keySet
  def currents = memory.toSet

  def === (other:State[L, V]):Boolean = (currents diff other.currents).isEmpty
  def =!= (other:State[L, V]):Boolean = !(this === other)
  def := (ls:Seq[(L, V)]):Unit = ls foreach { case (l, v) => setUpdate(l, v) }
  def := (ls:Set[(L, V)]):Unit = ls foreach { case (l, v) => setUpdate(l, v) }
}

/**
  *
  * @param sname
  * @tparam V
  */
class NullaryState[V](sname:String) extends State[Int, V](sname)
{
  //def apply():State[Int, V]#Mutator = super.apply(0)
  override def setUpdate(location:Int, value:V):Unit = super.setUpdate(0, value)
  override def toString = super.apply(0).get.toString

  def := (value:V):Unit = super.apply(0) := value
  def === (other:V) = super.apply(0).get == other
  def =!= (other:V) = super.apply(0).get != other
}


/**
  *
  */
object State
{
  def valueOf[L, V](a:State[L, V]):Set[(L, V)] = a.currents
  def valueOf[L, V](a:State[L, V]#Mutator):V = a.get
  def valueOf[V](a:NullaryState[V]):V = a.apply(0).get
  def apply[L, V](name:String) = new State[L, V](name)
  def apply[V](name:String) = new NullaryState[V](name)
}

