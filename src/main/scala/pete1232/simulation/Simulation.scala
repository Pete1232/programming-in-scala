package pete1232.simulation

abstract class Simulation {
  type Action = () => Unit

  //models an action to be executed at a specified time
  case class WorkItem(time: Int, action: Action)

  private var curTime = 0
  def currentTime: Int = curTime

  private var agenda: List[WorkItem] = Nil

  private def insert(ag: List[WorkItem], item: WorkItem): List[WorkItem] = {
    //if list is empty or this item is older than the head then prefix this item
    if(ag.isEmpty || item.time < ag.head.time) item :: ag
      // else it should go further down the stack
    else ag.head :: insert(ag.tail, item)
  }

  //add an item to agenda to do block after a delay
  //block is a by-name param, not executed when passed to afterDelay
  def afterDelay(delay: Int)(block: => Unit) = {
    val item = WorkItem(currentTime + delay, () => block)
    agenda = insert(agenda, item)
  }

  private def next() = {
    //this will always match something when called - but in theory could be called on an empty list and match nothing
    (agenda: @unchecked) match {
        //pattern match an item followed by more items (the rest)
      case item :: rest =>
        agenda = rest
        //update the time
        curTime = item.time
        item.action()
    }
  }

  def run() = {
    afterDelay(0) {
      println(s"*** simulation started, time = $currentTime ***")
    }
    //why next can be unchecked
    while(!agenda.isEmpty) next()
  }
}
