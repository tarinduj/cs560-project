package main

import interpreter._
import enumerator._

object Main extends App {
  val env = Map("x" -> BitVector("10000000"))
  val expr = BVAdd(Var("x"), BitVector("11111111"))
  println(BitVectorInterpreter.eval(expr, env))

  val bank = new Bank()
  bank.growTo(5)
  for (i <- 0 to 5) {
    println(s"Size $i: ${bank.sizes(i).size}")
    println(s"Size $i: ${bank.sizes(i).take(5)}")
  }
}
