package scalot

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
                  var docType: String = "",
                  id: String = Random.alphanumeric.take(20).mkString) {

def receiveOperation(op: Operation): Option[Operation] = {
op.revision > 0 match {
case true =>
val droped = operations.dropRight(op.revision - 1) // take all operations that happened concurrently
if (droped.nonEmpty) {
val resolved = droped.foldRight(op)((curr, res) => {
val result = Operation.transform(res, curr)
require(result.isDefined)
result.get.prime1
}
).copy(revision = operations.length + 1, id = op.id)
str = resolved.applyTo(str).get // apply operation to our text
operations = resolved :: operations // store operation in history
Some(resolved)
} else {
str = op.applyTo(str).get // apply operation to out text
operations = op :: operations // store operation in history
Some(op)
}
case _ => None
}
}
}