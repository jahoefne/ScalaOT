import org.scalatest._
import scalaot.ScalaOT
import scalaot.ScalaOT.TransformedPair


class ScalaOTTests extends FunSuite {

  test("Chaining Components of the same type does not increase the total number of components "){
    val op1 = ScalaOT.Operation()
      .insert("abc")
      .skip(2)
      .delete(2)

    val op2 = ScalaOT.Operation()
      .insert("a")
      .insert("b")
      .insert("c")
      .skip(1)
      .skip(1)
      .delete(1)
      .delete(1)

    assert(op2.ops.length == op1.ops.length, "Number of components in chained operations must not differ")
    assert(op2.baseLength == op1.baseLength, "BaseLength should be equal")
    assert(op2.targetLength == op1.targetLength, "TargetLength should be equal")
  }

  test("An OT with two clients creating a collision should resolvable by a simple transformation"){
    val start = "Hello World"
    val op1 = ScalaOT.Operation().insert("Start ").delete(11).insert(" End")
    val op2 = ScalaOT.Operation().skip(6).insert("Middle ").skip(5)

    val res1 = op1.apply(start)
    val res2 = op2.apply(start)

    info(s"Start String '$start")
    info(s"Operation1 -> ${op1.toString}")
    info(s"\tgives -> $res1")
    info(s"Operation2 -> ${op1.toString}")
    info(s"\tgives -> $res2")

    val TransformedPair(prime1, prime2) = ScalaOT.Operation.transform(op1,op2).get

    val resolved1 = prime1(res2.get)
    assert(resolved1.isDefined, "resolved1 must not be None")
    info(s"Reolver 1 -> ${prime1.toString}")
    info(s"\tResolves -> $resolved1")

    val resolved2 = prime2(res1.get)
    assert(resolved2.isDefined,"resolved2 must not be None" )
    info(s"Reolver 2 -> ${prime2.toString}")
    info(s"\tResolves -> $resolved2")

    assert(resolved1 == resolved2)
  }
}
