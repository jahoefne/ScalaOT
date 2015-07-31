package main

import java.util.UUID

import scalot.Documents.{ClientDocument, ServerDocument}
import scalot.Scalot.Operation


object Main {

  class Server(clients: Seq[TestClient]) {
    val doc = ServerDocument("")

    def receiveOperation(rev: Int, op: Operation) = {

      val res = doc.receiveOperation(rev, op) match {
        case Some(res) =>
          for (c <- clients) {
            c.applyServer(res)
          }
        case _ =>
      }

    }
  }


  class TestClient(revision: Int = 0, override val uuid: String = "foo", str: String = "")
    extends ClientDocument(revision, str, uuid) {
    var server: Server = new Server(Seq[TestClient]())

    override def sendOp(op: Operation, rev: Int) = server.receiveOperation(rev, op)
  }

  def main(args: Array[String]) {

    val client1 = new TestClient(0, "foo", "")
    val client2 = new TestClient(0, "foo", "")
    val server = new Server(Seq[TestClient](client1, client2))

    client1.server = server
    client2.server = server

    println("Cl1" + client1.str)
    println("Cl2" + client2.str)

    val op1 = new Operation().insert("Hello World")
    client1.applyClient(op1)

    val op2 = new Operation().insert("Foo")
    client2.applyClient(op2)

    println("Cl1 " + client1.str)
    println("Cl2 " + client2.str)
    println("Srv " + server.doc.str)

  }
}
