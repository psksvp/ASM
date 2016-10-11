package psksvp.AbstractStateMachine.Core

/**
  * Created by psksvp on 1/06/2016.
  */
abstract class Machine(val name:String)
{
  implicit def mutator2Value[L, V](a:State[L, V]#Mutator):V = State.valueOf(a)
  implicit def state2Set[L, V](a:State[L, V]):Set[(L, V)] = State.valueOf(a)
  implicit def nullaryState2Value[V](s:NullaryState[V]):V = State.valueOf(s)

  private var stateList:List[State[_, _]] = Nil

  def state[L, V](name:String):State[L, V]=
  {
    val s = State[L, V](name)
    stateList = stateList :+ s
    s
  }

  def state[V](name:String):NullaryState[V]=
  {
    val s = State[V](name)
    stateList = stateList :+ s
    s
  }


  def forall[L, V](s:State[L, V], withProperty:L=>Boolean = (l:L) => true)
                  (r:(L)=>Unit): Unit =
  {
    s.locations.foreach
    {
      case l if withProperty(l) => r(l)
    }
  }

  def skip:Unit=
  {

  }

  def listOfStates:List[State[_,_]] = stateList

  // must override the below functions

  def init:Boolean
  def main:Unit
}
