package okscala

object Experiment {
  def apply() = {
    case class ThingWithType() {
      type T = Int
    }

    val x = ThingWithType()
    val y = f[Int]
    println(y)
  }

  def f[U](): Seq[U] = {
    Seq[U]()
  }

}
