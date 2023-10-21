object Main extends App {
  val env = Map("x" -> BitVector("10000000"))
  val expr = BVAdd(Var("x"), BitVector("11111111"))
  println(BitVectorInterpreter.eval(expr, env))
}