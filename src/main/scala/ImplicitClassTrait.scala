object O {

  trait HasMessage[T] {
    def message(x: T): String
  }

  def process[T: HasMessage](x: T, processor: T => Unit): String = {
    processor(x)
    val hasMessage = implicitly[HasMessage[T]]
    hasMessage.message(x)
  }

  // Now we want to call `process` on a value of type `Structure`.

  case class Structure(msg: String)
  def processor(x: Structure): Unit = println(x)

  // A `Structure` does have a message within it. What we want to do is process the `Structure`,
  // while passing instructions about how to extract the message.

  implicit object HasMessageStructure extends HasMessage[Structure] {
    def message(x: Structure): String = x.msg
  }

  process[Structure](Structure("msg"), processor)

}
