package ui

import interpreter._
import enumerator._
import symbolicinterpreter._
import constraints._

import z3.scala._

import scala.collection.mutable.{Set => MutSet}

type Examples = Map[Map[String, ConcreteBitVector], ConcreteBitVector]

object UI extends App {
  def getSingleExample(examples: Examples): Examples = {
    def assignmentLoop(assigns: Map[String, ConcreteBitVector]): (Map[String, ConcreteBitVector], ConcreteBitVector) = {
      val response = scala.io.StdIn.readLine()
      val assignRegex = raw"([a-zA-Z0-9]+)\s*=\s*([01]+)".r
      val responseRegex = raw"([01]+)".r
      response match
        case assignRegex(name, bitvector) =>
          val assignment = name -> ConcreteBitVector(bitvector)
          assignmentLoop(assigns + assignment)
        case responseRegex(bitvector) =>
          (assigns, ConcreteBitVector(bitvector))
        case _ =>
          println("Invalid input. Try again.")
          assignmentLoop(assigns)
    }

    val (assigns, output) = assignmentLoop(Map.empty)
    examples + (assigns -> output)
  }

  def exampleLoop(examples: Examples): Examples = {
    println("Enter another example? (y/n)")
    val response = scala.io.StdIn.readLine()
    response match {
      case "y" =>
        println("Enter some assignments of the form 'name = bitvector', and then the desired output by itself")
        exampleLoop(getSingleExample(examples))
      case "n" => examples
      case _ =>
        println("Invalid response. Try again.")
        exampleLoop(examples)
    }
  }

  //val examples = Map(Map("x" -> ConcreteBitVector("010")) -> ConcreteBitVector("1"), Map("x" -> ConcreteBitVector("101")) -> ConcreteBitVector("10"))
  val examples = exampleLoop(Map.empty)
  println(s"Examples: $examples")

  val variables = MutSet(examples.head._1.keySet.toSeq.map(ConcreteVar(_)): _*)
  val bank = new Bank(variables)
  bank.growTo(7)

  val goodPrograms = bank.matchingPrograms(examples)
  println(s"There are ${goodPrograms.size} good programs.")

  // the smallest program is usually pretty general
  // val smallestProgram = goodPrograms.minBy(ConcreteBitVectorInterpreter.size(_))
  // println(s"Smallest program: $smallestProgram")

  filterLoop(goodPrograms)
  def filterLoop(goodPrograms: Set[ConcreteB]): Unit = {
    val progList = goodPrograms.toList
    val smallGoodPrograms = progList.sortBy(ConcreteBitVectorInterpreter.size(_)).take(70).toSet
    val bestInput: Map[String, ConcreteBitVector] = Constraints.findBestInput(smallGoodPrograms, examples.head._1.keySet)
    val possibleOutputs = progList.map(ConcreteBitVectorInterpreter.eval(_, bestInput)).toList

    println(s"Select the desired output for input $bestInput with 'select n', or enter it manually:")
    val selectRegex = raw"select\s*([0-9]+)".r
    val inputRegex = raw"([01]+)".r
    for ((output, i) <- possibleOutputs.take(15).zipWithIndex) {
      println(s"$i: $output")
    }

    val chosenOutput = scala.io.StdIn.readLine().trim match {
      case selectRegex(response) =>
        val index = response.toInt
        possibleOutputs(index)
      case inputRegex(response) =>
        response
      case _ =>
        println("Invalid input. Try again.")
        ???
    }

    println(s"Selected output: $chosenOutput")
    val filteredPrograms = goodPrograms.filter(ConcreteBitVectorInterpreter.eval(_, bestInput) == chosenOutput)
    val smallestFilteredProgram = filteredPrograms.minByOption(ConcreteBitVectorInterpreter.size(_))
    println(s"${filteredPrograms.size} programs remain. Smallest: $smallestFilteredProgram")
    filterLoop(filteredPrograms)
  }
}
