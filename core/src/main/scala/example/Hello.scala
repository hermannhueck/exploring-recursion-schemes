package example

import scala.util.chaining._

object Hello extends Greeting with util.App {

  greeting pipe println
}

trait Greeting {
  lazy val greeting: String = "hello"
}
