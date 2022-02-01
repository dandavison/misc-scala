package okscala

object Experiment {
  def apply() = {
    case class ThingWithType() {
      type T = Int
    }

    val x = ThingWithType()
    val y = f[x.T]
    println(y)
  }

  def f[U](): Seq[U] = {
    Seq[U]()
  }

}
