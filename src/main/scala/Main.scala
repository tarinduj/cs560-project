object Main extends App {
  val expr1 = BVAdd(BitVector("10000000"), BitVector("01111111"))
  println(BitVectorInterpreter.eval(expr1))
  val expr2 = BVSub(BitVector("10000000"), BitVector("01111111"))
  println(BitVectorInterpreter.eval(expr2))
  val expr3 = BVAnd(BitVector("10000000"), BitVector("01111111"))
  println(BitVectorInterpreter.eval(expr3))
  val expr4 = BVOr(BitVector("10000000"), BitVector("01111111"))
  println(BitVectorInterpreter.eval(expr4))
  val expr5 = BVXor(BitVector("10000000"), BitVector("01111111"))
  println(BitVectorInterpreter.eval(expr5))
  val expr6 = BVNot(BitVector("10000000"))
  println(BitVectorInterpreter.eval(expr6))
}