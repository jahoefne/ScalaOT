package plainOt

object App {
  def main(args: Array[String]) {

    val op = new PlainOT.Operation().skip(6).insert("Mama").delete(5).insert(" Mia!")

    println(op.baseLength)

    val applied = op.apply("hello world").get
    println(s"'$applied' ${applied.length}  ${op.targetLength}")

  }
}
