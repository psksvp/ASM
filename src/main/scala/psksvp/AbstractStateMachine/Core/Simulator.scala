package psksvp.AbstractStateMachine.Core

/**
  * Created by psksvp on 1/06/2016.
  */

class Simulator(machine:Machine, visualizer: Option[Visualizer] = None)
{
  if(false == machine.init)
    sys.error(s"Init rule ${machine.name} fail")
  else
    update()

  private def update():Unit = machine.listOfStates.foreach(_.commit)


  def run(nStep:Int = -1):Unit=
  {
    require(machine.listOfStates != Nil)

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
    machine.main
    val fixed = hasReachedFixedPoint
    update()
    visualizer match
    {
      case Some(v) => v.view(machine.listOfStates, s)
      case _       =>
    }
    fixed
  }

  def hasReachedFixedPoint:Boolean =
  {
    val fixed = for(s <- machine.listOfStates) yield s.isFixedPoint
    fixed.reduce(_ & _)
  }
}
