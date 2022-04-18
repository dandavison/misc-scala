// type Structure1 = X
// type Structure2 = (Int, X)

object O {

  trait HasMessage {
    def message(): String
  }

  def process[H <: HasMessage](h: H, processor: H => Unit): String = {
    processor(h)
    h.message
  }

  // Now we want to call `process` on a value of type `X`.

  case class X(msg: String)
  def processor1(x: X): Unit = println(x)

  // An X does have a message within it. What we want to do is process the `X`, while passing
  // instructions about how to extract the message.

  implicit class ToHasMessage1(val x: X) extends HasMessage {
    def message(): String = x.msg
  }

  // Clearly neither of these will compile:
  process[X](X("msg 1"), processor1(_)) // Nothing is implementing the HasMessage trait
  process[HasMessage](X("msg 1"), processor1(_)) // Type mismatch in both arguments

  // (2) supplying the type `(Int, X)` as the type parameter `H`
  // def processor2(arg: (Int, X)): Unit = println(arg)
  // val s2 = (7, X("msg 2"))
  // implicit class ToHasMessage2(val s: (Int, X)) extends HasMessage {
  //   def message(): String = s._2.msg
  // }

}
