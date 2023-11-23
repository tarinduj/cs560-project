package main

import interpreter._
import enumerator._
import symbolicinterpreter._

import z3.scala._

import scala.collection.mutable.Set

object Main extends App {
  val env = Map("x" -> ConcreteBitVector("10000000"))
  val expr = ConcreteBVOr(ConcreteVar("x"), ConcreteBitVector("01111111"))
  println(ConcreteBitVectorInterpreter.eval(expr, env))

  val expr2 =  ConcreteBitVector("11111110")


  val ctx = new Z3Context(Map("MODEL" -> "true"))
  
  // Evaluate both expressions to Z3 ASTs
  val ast1 = SymbolicBitVectorInterpreter.eval(expr, env, ctx)
  val ast2 = SymbolicBitVectorInterpreter.eval(expr2, env, ctx)

  // Create a solver
  val solver = ctx.mkSolver()

  // Assert equality
  val equal = ctx.mkEq(ast1, ast2)
  solver.assertCnstr(equal)

  // Check for satisfiability
  val result = solver.check()


  result match {
    case Some(false) => println("UNSAT")
    case Some(true) => {
      println("SAT")

      // Get the model
      val model = solver.getModel()
    }
    case None => println("UNKNOWN")
  }

  // Delete the context
  ctx.delete()



  // {
  //   println("==== Without Pruning ====")
  //   val bank = new Bank(variables = Set(Var("x")), inputs = None)
  //   bank.growTo(5)
  //   for (i <- 0 to 5) {
  //     println(s"Size $i: ${bank.sizes(i).size}")
  //     println(s"Size $i: ${bank.sizes(i).take(5)}")
  //   }
  // }

  // {
  //   println("==== With Pruning ====")
  //   val bank = new Bank(variables = Set(Var("x")), inputs = Some(Array(Map("x" -> BitVector("10000000")))))
  //   bank.growTo(10)
  //   for (i <- 0 to 10) {
  //     println(s"Size $i: ${bank.sizes(i).size}")
  //     println(s"Size $i: ${bank.sizes(i).take(5)}")
  //   }
  //   println(s"Outputs: ${bank.outputs.get.map(_.mkString(","))}")
  // }
}
