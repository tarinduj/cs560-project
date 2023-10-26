package main

import interpreter._
import enumerator._

import scala.collection.mutable.Set

object Main extends App {
  val env = Map("x" -> BitVector("10000000"))
  val expr = BVAdd(Var("x"), BitVector("11111111"))
  println(BitVectorInterpreter.eval(expr, env))

  {
    println("==== Without Pruning ====")
    val bank = new Bank(variables = Set(Var("x")), inputs = None)
    bank.growTo(5)
    for (i <- 0 to 5) {
      println(s"Size $i: ${bank.sizes(i).size}")
      println(s"Size $i: ${bank.sizes(i).take(5)}")
    }
  }

  {
    println("==== With Pruning ====")
    val bank = new Bank(variables = Set(Var("x")), inputs = Some(Array(Map("x" -> BitVector("10000000")))))
    bank.growTo(10)
    for (i <- 0 to 10) {
      println(s"Size $i: ${bank.sizes(i).size}")
      println(s"Size $i: ${bank.sizes(i).take(5)}")
    }
    println(s"Outputs: ${bank.outputs.get.map(_.mkString(","))}")
  }
}
