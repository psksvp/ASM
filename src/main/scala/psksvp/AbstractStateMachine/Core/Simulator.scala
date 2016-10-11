package psksvp.AbstractStateMachine.Core

/**
  * Created by psksvp on 1/06/2016.
  */

class Simulator(rule:Machine, visualizer: Option[Visualizer] = None)
{
  private var states:List[State[_,_]] = rule.listOfStates
  if(false == rule.init)
    sys.error(s"Init rule ${rule.name} fail")
  else
    update()

  private def update():Unit = states.foreach(_.commit)

  def addState(s:State[_,_]*):Unit =
  {
    states = psksvp.removeDuplicate(s.toList ::: states)
  }

  def run(nStep:Int = -1):Unit=
  {
    require(states != Nil)

    var stepCount = 0
    var fixed = step(0)
    var keepRunning = true
    while(!fixed && keepRunning)
    {
      stepCount = stepCount + 1
      fixed = step(stepCount)
      keepRunning = if(-1 != nStep) stepCount < nStep else true
    }
  }

  def step(s:Int):Boolean =
  {
    rule.main
    val fixed = hasReachedFixedPoint
    update()
    visualizer match
    {
      case Some(v) => v.view(rule.listOfStates, s)
      case _       =>
    }
    fixed
  }

  def hasReachedFixedPoint:Boolean =
  {
    val fixed = for(s <- states) yield s.isFixedPoint
    fixed.reduce(_ & _)
  }
}
