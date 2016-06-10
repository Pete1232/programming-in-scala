package pete1232.currencies

object Converter {
  var exchangeRate = Map(
    "USD" -> Map("USD" -> 1.0, "GBP" -> 1.25),
    "GBP" -> Map("USD" -> 0.8, "GBP" -> 1.0)
  )
}

abstract class CurrencyZone {
  // Currency must be a sub-type of AbstractCurrency
  // also sets this as an alias
  type Currency <: AbstractCurrency
  val CurrencyUnit: Currency

  // each CurrencyZone holds a factory for the Currency itself
  def make(amount: Long): Currency

  // definition for the AbstractCurrency class
  abstract class AbstractCurrency {
    val amount: Long
    def designation: String
    def +(that: Currency): Currency = make(this.amount + that.amount)
    def *(x: Double): Currency = make((this.amount * x).toLong)
    // creates a new amount of this Currency from another Currency
    def from(another: CurrencyZone#AbstractCurrency): Currency = {
      make(math.round(
        another.amount.toDouble * Converter.exchangeRate(another.designation)(this.designation)
      ))
    }
    private def decimals(n: Long): Int = if (n == 1) 0 else {1 + decimals(n / 10)}
    override def toString = ((amount.toDouble / CurrencyUnit.amount.toDouble) formatted ("%." + decimals(CurrencyUnit.amount) + "f") + " " + designation)
  }
}

object US extends CurrencyZone {
  abstract class Dollar extends AbstractCurrency {
    def designation = "USD"
  }
  type Currency = Dollar
  def make(cents: Long) = new Dollar {
    override val amount: Long = cents
  }
  val Cent = make(1)
  val Dollar = make(100)
  val CurrencyUnit = Dollar
}

object UK extends CurrencyZone {
  abstract class Pound extends AbstractCurrency {
    def designation = "GBP"
  }
  type Currency = Pound
  def make(pence: Long) = new Pound {
    override val amount: Long = pence
  }
  val Pence = make(1)
  val Pound = make(100)
  val CurrencyUnit = Pound
}

object CurrencyTest extends App {
  var current: CurrencyZone#AbstractCurrency =_
  current = US.Dollar from UK.Pound
  println(current)
  current = UK.Pound from current
  println(current)
}
