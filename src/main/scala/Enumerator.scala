package enumerator

import interpreter._

import scala.collection.mutable.{ArrayBuffer, Set}

class Bank(val variables: Set[ConcreteVar] = Set(), val inputs: Option[Array[Map[String, ConcreteBitVector]]] = None) {
  val sizes = ArrayBuffer[Set[ConcreteB]](Set(ConcreteZero, ConcreteOne) ++ variables.map(_.asInstanceOf[ConcreteB]))
  var currentSize = 1
  val outputs: Option[Set[Array[ConcreteBitVector]]] = inputs.map(_ => Set.empty)

  def growTo(size: Int) = {
    while (sizes.length <= size) {
      sizes += Set.empty[ConcreteB]
    }

    for (czize <- currentSize to size) {
      val unaryConstructors: List[ConcreteB => ConcreteB] = List(ConcreteBVNot)
      val binaryConstructors: List[(ConcreteB, ConcreteB) => ConcreteB] = List(ConcreteBVAdd, ConcreteBVSub, ConcreteBVOr, ConcreteBVAnd, ConcreteBVXor)

      val unaryExps: Iterable[ConcreteB] = for {
        subExp <- sizes(czize - 1)
        op <- unaryConstructors
      } yield op(subExp)

      val binaryExps: Iterable[ConcreteB] = for {
        lSize <- 1 to (czize - 1)
        rSize = czize - lSize
        lExp <- sizes(lSize - 1)
        rExp <- sizes(rSize - 1)
        op <- binaryConstructors
      } yield op(lExp, rExp)

      if (inputs.isDefined) {
        for (exp <- unaryExps ++ binaryExps) {
          val envs = inputs.get
          val results: Array[ConcreteBitVector] = envs.map(ConcreteBitVectorInterpreter.eval(exp, _)).map(ConcreteBitVector(_))
          if (!outputs.get.exists(_.sameElements(results))) {
            sizes(czize) += exp
            outputs.get += results
          }
        }
      } else {
        // no pruning
        for (exp <- unaryExps ++ binaryExps) {
          sizes(czize) += exp
        }
      }
    }
  }
}
