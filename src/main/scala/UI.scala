package ui

import interpreter._
import enumerator._
import symbolicinterpreter._

import z3.scala._

import scala.collection.mutable.{Set => MutSet}

type Examples = Map[Map[String, ConcreteBitVector], ConcreteBitVector]

object UI extends App {
  def getSingleExample(examples: Examples): Examples = {
    def assignmentLoop(assigns: Map[String, ConcreteBitVector]): Map[String, ConcreteBitVector] = {
      val response = scala.io.StdIn.readLine()
      val assignRegex = raw"([a-zA-Z0-9]+)\s*=\s*([01]+)".r
      response match
        case "n" => assigns
        case assignRegex(name, bitvector) =>
          val assignment = name -> ConcreteBitVector(bitvector)
          assignmentLoop(assigns + assignment)
        case _ =>
          println("Invalid assignment. Try again.")
          assignmentLoop(assigns)
    }

    val assigns = assignmentLoop(Map.empty)
    println("Enter the output of the program: ")
    val output = ConcreteBitVector(scala.io.StdIn.readLine())
    examples + (assigns -> output)
  }

  def exampleLoop(examples: Examples): Examples = {
    println("Enter another example? (y/n)")
    val response = scala.io.StdIn.readLine()
    response match {
      case "y" => 
        println("Enter some assignments of the form 'name = bitvector', or 'n' to exit: ")
        exampleLoop(getSingleExample(examples))
      case "n" => examples
      case _ =>
        println("Invalid response. Try again.")
        exampleLoop(examples)
    }
  }

  val examples = exampleLoop(Map.empty)
  println(s"Examples: $examples")

  val variables = MutSet(examples.head._1.keySet.toSeq.map(ConcreteVar(_)): _*)
  val bank = new Bank(variables)
  bank.growTo(5) 

  val goodPrograms = bank.matchingPrograms(examples)
  val smallestProgram = goodPrograms.minBy(ConcreteBitVectorInterpreter.size(_))
  println(s"Smallest program: $smallestProgram")
}
