package pete1232.simulation

abstract class BasicCircuitSimulation extends Simulation {

  //constants modelling the delays in the physical system
  def InverterDelay: Int
  def AndDelay: Int
  def OrDelay: Int

  class Wire {
    private var sigVal = false
    //models a series of components on the wire - state to be updated whenever a change in the circuit is detected
    private var actions: List[Action] = Nil

    def getSignal = sigVal

    def setSignal(sig: Boolean) = {
      if(sig != sigVal) {
        sigVal = sig
        //execute each action with the new signal (every time the signal changes)
        actions foreach (_ ())
      }
    }

    def addAction(a: Action) = {
      actions = a :: actions
      //when a component is added the action is executed straight away
      a()
    }
  }

  def inverter(i: Wire, o: Wire) = {
    def invertAction() = {
      val inputSig = i.getSignal
      afterDelay(InverterDelay) {
        o setSignal(!inputSig)
      }
    }
    i addAction invertAction
  }

  def andGate(i1: Wire, i2: Wire, o: Wire) = {
    def andAction() = {
      val i1Sig = i1.getSignal
      val i2Sig = i2.getSignal
      afterDelay(AndDelay) {
        o setSignal(i1Sig & i2Sig)
      }
    }
    //add tp both so a change to either wire triggers re-computation
    i1 addAction andAction
    i2 addAction andAction
  }

  def orGate(i1: Wire, i2: Wire, o: Wire) = {
    def orAction() = {
      val i1Sig = i1.getSignal
      val i2Sig = i2.getSignal
      afterDelay(OrDelay) {
        o setSignal(i1Sig | i2Sig)
      }
    }
    i1 addAction orAction
    i2 addAction orAction
  }

  def probe(name: String, wire: Wire) = {
    def probeAction() = {
      println(s"$name $currentTime new-value = ${wire.getSignal}")
    }
    wire addAction probeAction
  }
}
