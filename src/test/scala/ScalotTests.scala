import org.scalatest._
import scalot.{Operation, TransformedPair, Component}
import scala.util.Random


class ScalotTests extends FunSuite with GivenWhenThen {

  def repeatRandomCount = 5000

  def randomString(length: Int): String = Random.alphanumeric.take(length).mkString

  def randomSeqWithSum(max: Int, curr: Seq[Int] = Seq[Int]()): Seq[Int] = curr.sum match {
    case x if x > max => randomSeqWithSum(max, curr.drop(1))
    case x if x < max => randomSeqWithSum(max, curr :+ Random.nextInt(10) + 1)
    case x if x == max => curr
  }

  def randomOpFor(str: String): Operation = {
    val numberz = randomSeqWithSum(str.length)
    var op = Operation()
    for (x <- numberz) {
      Random.nextBoolean() match {
        case true => op = op.delete(x)
        case false => op = op.skip(x)
      }
      if (Random.nextInt(2) == 0)
        op = op.insert(randomString(x))
    }
    op
  }

  test("Random Transform"){
    print("\nRandom transformation test")
    (1 to repeatRandomCount).foreach(x => {
      print(s" $x")
      val str = randomString(Random.nextInt(500)+5)
      val opA = randomOpFor(str)
      val opB = randomOpFor(str)

      val resA = opA(str).get
      val resB = opB(str).get

      val trans = Operation.transform(opA, opB).get
     // println(s"Input String Length ${str.length}")
     // println(s"Prime1: base(${trans.prime1.baseLength} should be ${resB.length}}) target(${trans.prime1.targetLength})  \n\t${trans.prime1}")
     // println(s"Prime2: base(${trans.prime2.baseLength} should be ${resA.length}) target(${trans.prime2.targetLength})  \n\t${trans.prime2}")

      val resAB = trans.prime2(resA)
      val resBA = trans.prime1(resB)
      //info(resAB)
      assert(resAB.isDefined && resBA.isDefined)
      assert(resAB == resBA)
    })
  }


  test("Random Inverse"){
    print(s"\nRandom Inverse Test")
    (1 to repeatRandomCount).foreach(x => {
      print(s" $x")
      val str = randomString(Random.nextInt(500)+5)
      val op1 = randomOpFor(str)

      val res1 = op1(str).get

      val inverse = op1.invert(str)

      //println(s"\nOriginal '$str'")
      //println(s"Inverted '${inverse(res1).get}'")
      assert(str == inverse(res1).get, "Operation should be reversable")
    })
  }

    test("Random Apply") {
      print(s"\nRandom Apply Test")
      (1 to repeatRandomCount).foreach(x => {
        print(s" $x")

        val str = randomString(Random.nextInt(500)+5)
       // Given(s"a Random String - $str")

       // And("a random operation for that string")
        val op1 = randomOpFor(str)
        assert(str.length == op1.baseLength, "Operation Size ")

       // Then("the Operation should be applicable to the string")
        val res = op1(str).get
        assert(res.length == op1.targetLength, "Actual target length doesn't match expected targetlength")
      })
    }

    test("Inverting an Operation") {
      val str = "Hello World"
     // println(s"\nOriginal String is '$str'")

      val op1 = Operation().insert("Hallo Welt!").delete(11)
     // println(s"Original Op is ${op1.toString}")

      val res = op1(str).get
     // println(s"\tResult String is '$res'")

      val invertedOp = op1.invert(str)
      //println(s"Inverted Op is ${invertedOp.toString}")

      val original = invertedOp(res).get
     // println(s"\tRestored Original String is '$original'")

      assert(original == str)
    }

    test("Random Compose Test") {
      print(s"\nStarting Compose Test")
      (1 to repeatRandomCount).foreach(retry => {
        print(s" $retry")
        val str = randomString(200)

        val op1 = randomOpFor(str)
        val op2 = randomOpFor(op1(str).get)

        val res1 = op1(str).get
        //info(s"\tstr = '$str'")
       // info(s"\tOp1 is ${op1.toString}")
       // info(s"\t\t res1 = Op1(str) = $res1")

        val res2 = op2(res1).get
       // info(s"\tOp2 is ${op2.toString}")
       // info(s"\t\t Op1(res) = $res2")

        val composed = op1.compose(op2).get
        val composedResult = composed(str)
       // info(s"\t\tComposed Operation is ${composed.toString}")
       // info(s"\t\t\t composed(str) = $composedResult")
      })
    }

/*
  val server = Server("")
  val client = Client("", 0)
  val client2 = Client("", 0)

  val op = Operation().insert("Hello World!")
  val op2 = Operation().skip(6).insert("Cruel ").skip(6)


  def handleServer(op: Option[Operation]): Unit = {
    op match {
      case Some(x) =>
        println(s"Sending ${x.id} to server!")
        server.receiveOperation(x) match {
          case Some(resp) =>
            println(s"\t Server Responded! ${resp.id}, sent ${x.id} $resp")
            val res1 = client.applyRemote(resp)
            val res2 = client2.applyRemote(resp)
            handleServer(res1)
            handleServer(res2)
          case _ =>
        }
      case _ =>
        println("Nothing to send to server!")
    }
  }

  val res = client.applyLocal(op)
  val res2 = client.applyLocal(op2)

  val op3 = Operation().insert("Foo")
  val res3 = client2.applyLocal(op3)
  println(res3)
  handleServer(res)
  handleServer(res3)
  handleServer(res2)

  println(s"Server ${server.str}")
  println(s"Client ${client.str}")
  println(s"Client2 ${client2.str}")
  */



    test("Chaining Components of the same type does not increase the total number of components ") {
      val op1 = Operation()
        .insert("abc")
        .skip(2)
        .delete(2)

      val op2 = Operation()
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

    test("An OT with two clients creating a collision should resolvable by a simple transformation") {
      val start = "Hello World"
      val op1 = Operation().insert("Start ").delete(11).insert(" End")
      val op2 = Operation().skip(6).insert("Middle ").skip(5)

      val res1 = op1.apply(start)
      val res2 = op2.apply(start)

      /*info(s"Start String '$start")
      info(s"Operation1 -> ${op1.toString}")
      info(s"\tgives -> $res1")
      info(s"Operation2 -> ${op1.toString}")
      info(s"\tgives -> $res2")*/

      val TransformedPair(prime1, prime2) = Operation.transform(op1, op2).get

      val resolved1 = prime1(res2.get)
      assert(resolved1.isDefined, "resolved1 must not be None")
     /* info(s"Reolver 1 -> ${prime1.toString}")
      info(s"\tResolves -> $resolved1")*/

      val resolved2 = prime2(res1.get)
      assert(resolved2.isDefined, "resolved2 must not be None")
      /*info(s"Reolver 2 -> ${prime2.toString}")
      info(s"\tResolves -> $resolved2")*/

      assert(resolved1 == resolved2)
    }
  }
