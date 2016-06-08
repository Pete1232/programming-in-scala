package pete1232.simulation

//can implement at runtime by adding the MySimulation class to the interpreter
//alternatively just run it as an app
abstract class CircuitSimulation extends BasicCircuitSimulation {

  def halfAdder(a: Wire, b: Wire, s: Wire, c: Wire) = {
    val d, e = new Wire
    orGate(a, b, d)
    andGate(a, b, c)
    inverter(c, e)
    andGate(d, e, s)
  }

  def fullAdder(a: Wire, b: Wire, cin: Wire, sum: Wire, cout: Wire) = {
    val s, c1, c2 = new Wire
    halfAdder(a, cin, s, c1)
    halfAdder(b, s, sum, c2)
    orGate(c1, c2, cout)
  }
}

object MySimulation extends CircuitSimulation with App {
  def InverterDelay = 1
  def AndDelay = 3
  def OrDelay = 5

  val input1, input2, sum, carry = new Wire

  probe("sum", sum)

  probe("carry", carry)

  halfAdder(input1, input2, sum, carry)

  input1 setSignal true

  run()

  input2 setSignal true

  run()
}