package enumerator

import interpreter._

import scala.collection.mutable.{ArrayBuffer, Set}

class Bank() {
  val sizes = ArrayBuffer[Set[B]](Set(Zero, One))
  var currentSize = 1

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

      sizes(czize) ++= unaryExps
      sizes(czize) ++= binaryExps
    }
  }
}
