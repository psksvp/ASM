package psksvp.AbstractStateMachine.Core

/**
  * Created by psksvp on 25/06/2016.
  */
trait Visualizer
{
  def view(states:List[State[_, _]], nthStep:Int):Unit
}
