package enumerator

import interpreter._

import scala.collection.mutable.{ArrayBuffer, Set}

class Bank(val variables: Set[Var] = Set(), val inputs: Option[Array[Map[String, BitVector]]] = None) {
  val sizes = ArrayBuffer[Set[B]](Set(Zero, One) ++ variables.map(_.asInstanceOf[B]))
  var currentSize = 1
  val outputs: Option[Set[Array[BitVector]]] = inputs.map(_ => Set.empty)

  def growTo(size: Int) = {
    while (sizes.length <= size) {
      sizes += Set.empty[B]
    }

    for (czize <- currentSize to size) {
      val unaryConstructors: List[B => B] = List(BVNot)
      val binaryConstructors: List[(B, B) => B] = List(BVAdd, BVSub, BVOr, BVAnd, BVXor)

      val unaryExps: Iterable[B] = for {
        subExp <- sizes(czize - 1)
        op <- unaryConstructors
      } yield op(subExp)

      val binaryExps: Iterable[B] = for {
        lSize <- 1 to (czize - 1)
        rSize = czize - lSize
        lExp <- sizes(lSize - 1)
        rExp <- sizes(rSize - 1)
        op <- binaryConstructors
      } yield op(lExp, rExp)

      if (inputs.isDefined) {
        for (exp <- unaryExps ++ binaryExps) {
          val envs = inputs.get
          val results: Array[BitVector] = envs.map(BitVectorInterpreter.eval(exp, _)).map(BitVector(_))
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
