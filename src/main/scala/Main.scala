object Main extends App {
  val expr = BVAdd(BitVector("10000000"), BitVector("01111111"))
  println(BitVectorInterpreter.eval(expr))
}