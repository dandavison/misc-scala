package okscala

class OkRuntimeClassSyntax {
  println("OkRCS class")
}

object OkRuntimeClassSyntax {
  def apply() = {
    println("OkRCS object")
    f
  }

  def f() = println("f")

}
