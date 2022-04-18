package okscala

case class CaseClassWithAlternateConstructor(myField: String) {
  println("In class constructor")
}

object CaseClassWithAlternateConstructor {
  def apply(myField: String): CaseClassWithAlternateConstructor = {
    println("In CompanionObject.apply")
    new CaseClassWithAlternateConstructor(myField)
  }
}
