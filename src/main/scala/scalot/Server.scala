package scalot

import java.util.UUID

case class Server(var str: String,
                          val uuid: String = UUID.randomUUID().toString,
                          var operations: List[Operation] = List[Operation]()) {

  def receiveOperation(op: Operation): Option[Operation] = {
    println(s"SERVER ${op}")
    op.revision > 0 match {
      case true =>
        println("Revision: "+op.revision)
        println(operations)
        val droped = operations.drop(op.revision-1) // take all operations that happened concurrently
        if (droped.nonEmpty) {
          println("Dropped is not empty!")

          // and resolve all conflicts recursively
          val resolved = droped.foldRight(op)((curr, res) =>  {
             println("Transform " + curr + " and "+ res)
            Operation.transform(res, curr).get.prime1
          })

          println(s"resolved ${resolved.baseLength}  ${str.length}")

          str = resolved(str).get // apply operation to our text
          println("Server has: "+ str)
          println("Resolved:" + resolved)
          operations = resolved :: operations  // store operation in history
          Some(resolved)
        } else {
          str = op(str).get // apply operation to out text
          operations = op :: operations  // store operation in history
          println("No need to transform return "+op)
          Some(op)
        }
      case _ =>
        println(s"Catch all ${op.revision} ${operations.length}")
        None
    }
  }
}