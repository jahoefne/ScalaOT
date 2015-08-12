package scalot

import java.util.UUID

import scala.util.Random

/**
 * Represents a server document instance, must be created once per document
 * @param str the initial content of the document
 * @param uuid optionally specify a uuid
 * @param operations the history of operation that was performed on the document
 */
case class Server(var str: String,
                  var operations: List[Operation] = List[Operation](),
                  var title: String = "",
                  id: String = Random.alphanumeric.take(20).mkString) {

  def receiveOperation(op: Operation): Option[Operation] = {
    op.revision > 0 match {
      case true =>
        val droped = operations.drop(op.revision-1) // take all operations that happened concurrently
        if (droped.nonEmpty) {
          // and resolve all conflicts recursively
          val resolved = droped.foldRight(op)((curr, res) =>  {
            Operation.transform(res, curr).get.prime1
          })
          str = resolved.applyTo(str).get // apply operation to our text
          operations = resolved :: operations  // store operation in history
          Some(resolved)
        } else {
          str = op.applyTo(str).get // apply operation to out text
          operations = op :: operations  // store operation in history
          Some(op)
        }
      case _ =>
        None
    }
  }
}