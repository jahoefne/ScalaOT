import scalot.ClientFSM.{Synchronized, AwaitWithBuffer, AwaitConfirm}
import scalot._
import utest._
import utest.ExecutionContext.RunNow
import upickle.default._
import scala.util.Random


object ClientStateMachine {

  val test = TestSuite {

    /** Typical case if the document is edited by a single user. And that user triggers new operations before
      * the server can confirm the previous operations
      */
    "Sync -> AwaitConfirm -> AwaitWithBuff -> AwaitConfirm -> Sync" - {
      val server = Server("")
      val client1 = Client("", 0)

      val op1 = Operation().insert("Hello ")
      val op2 = Operation().skip(6).insert("World!")
      val op3 = Operation().skip(12).insert(" Foo")

      /**
       * Applying an operation on a client wich is in synchronized stats should move the client to
       * AwaitConfirm State and return an operation which should be sent to the server, but no operation which
       * should be applied to the view
       */
      val send1 = client1.applyLocal(op1)
      assert(send1.send.isDefined && send1.apply.isEmpty)
      assertMatch(client1.getState()) { case _: AwaitConfirm => }

      /**
       * Client is in awaiting state, until we got a confirmation for our op we don't have to send anything
       * to the server anymore, if we apply more operations on the server we should en up in awaitwithbuffer state
       */
      val send2 = client1.applyLocal(op2)
      val send3 = client1.applyLocal(op3)
      assert(send2.send.isEmpty && send3.send.isEmpty && send2.apply.isEmpty && send3.apply.isEmpty)
      assertMatch(client1.getState()) { case _: AwaitWithBuffer => }

      /** if we let the server receive send1 now */
      val response = server.receiveOperation(send1.send.get)

      /** as far as we know that's the first revision that exists */
      assert(send1.send.get.revision == 1)

      /** it should return a confirmation package with the same revision / uuid */
      assert(response.isDefined && response.get.id == send1.send.get.id && response.get.revision == send1.send.get.revision)
      /** Applying this confirmation to the client */
      val send4 = client1.applyRemote(response.get)

      /** Should shift the client in AwaitConfirm state and return a composition of the buffer (send2/send3) which should
        * be sent to the server to get a confirmation again. But nothing to apply to the view beacuse this operation
        * is our own op and was already optimisitcally applied before sending the op in the first place
        */
      assertMatch(client1.getState()) { case _: AwaitConfirm => }
      assert(send4.send.isDefined && send4.apply.isEmpty)

      /** server just confirmed that it's on revision 1 so we should send revision 2 now */
      assert(send4.send.get.revision == 2)


      /** If we send the combined buffer operation to the server now */
      val response2 = server.receiveOperation(send4.send.get)

      /** we should get a confirmation */
      assert(response2.isDefined)

      /** and the server should have the same string as the client */
      assert(client1.str == server.str)

      /** and the string must be the final string */
      assert(client1.str == "Hello World! Foo")

      /** and if we apply the servers response to the client */
      val send5 = client1.applyRemote(response2.get)

      /** we end up in synchonized state */
      assertMatch(client1.getState()) { case _: Synchronized => }

      /** and get nothing to send to the server and nothing to apply to the view */
      assert(send5.send.isEmpty && send5.apply.isEmpty)

      /** and out revision should be 2 */
      assert(client1.revision == 2)
    }

    /** Typical case if a single user is editing the document and only triggers few operations */
    "Sync -> AwaitConfirm -> Sync -> AwaitConfirm -> Sync" - {
      val server = Server("")
      val client1 = Client("", 0)

      val op1 = Operation().insert("Hello ")

      val send1 = client1.applyLocal(op1)
      assert(send1.send.isDefined && send1.apply.isEmpty)
      assertMatch(client1.getState()) { case _: AwaitConfirm => }

      /** as far as we know that's the first revision that exists */
      assert(send1.send.get.revision == 1)

      /** if we let the server receive send1 now */
      val response = server.receiveOperation(send1.send.get)

      /** it should return a confirmation package with the same revision / uuid */
      assert(response.isDefined && response.get.id == send1.send.get.id && response.get.revision == send1.send.get.revision)
      val send2 = client1.applyRemote(response.get)
      assert(send2.apply.isEmpty && send2.send.isEmpty)
      assertMatch(client1.getState()) { case _: Synchronized => }
      assert(client1.revision == 1)
      assert(client1.str == server.str && client1.str == "Hello ")

      val op2 = Operation().skip(6).insert("World!")
      val send3 = client1.applyLocal(op2)
      assert(send3.send.isDefined && send3.apply.isEmpty)
      assertMatch(client1.getState()) { case _: AwaitConfirm => }

      val response2 = server.receiveOperation(send3.send.get)

      /** it should return a confirmation package with the same revision / uuid */
      assert(response2.isDefined && response2.get.id == send3.send.get.id && response2.get.revision == send3.send.get.revision)
      val send4 = client1.applyRemote(response2.get)
      assert(send4.apply.isEmpty && send4.send.isEmpty)
      assertMatch(client1.getState()) { case _: Synchronized => }
      assert(client1.str == server.str && client1.str == "Hello World!")
    }


    /** Simple collision case
      * Client1 and Client2 edit the document at the same time, Client1's operation is received by the server
      * before client2's operation
      */
    "Conflict resolution test 1" - {
      val server = Server("")
      val client1 = Client("", 0)
      val client2 = Client("", 0)

      val op1 = Operation().insert("Hello ")
      val op2 = Operation().insert("World!")

      val send1 = client1.applyLocal(op1)
      assert(send1.send.isDefined && send1.apply.isEmpty)
      assertMatch(client1.getState()) { case _: AwaitConfirm => }

      /** as far as we know that's the first revision that exists */
      assert(send1.send.get.revision == 1)


      val send2 = client2.applyLocal(op2)
      assert(send2.send.isDefined && send2.apply.isEmpty)
      assertMatch(client2.getState()) { case _: AwaitConfirm => }

      /** as far as we know that's the first revision that exists */
      assert(send2.send.get.revision == 1)

      val response1 = server.receiveOperation(send1.send.get)
      assert(response1.isDefined && response1.get.id == send1.send.get.id && response1.get.revision == send1.send.get.revision)
      val applyRes1 = client1.applyRemote(response1.get)
      assert(applyRes1.send.isEmpty, applyRes1.apply.isEmpty)

      val applyRes2 = client2.applyRemote(response1.get)
      assert(applyRes2.send.isEmpty, applyRes2.apply.isDefined)

      val response2 = server.receiveOperation(send2.send.get)
      println(response2.get)
      assert(response2.isDefined)
      val applyRes3 = client1.applyRemote(response2.get)
      val applyRes4 = client2.applyRemote(response2.get)
      assert(client1.str == client2.str, client1.str == server.str)
    }



    /** A bit more complex collision case
      * Client1 and Client2 edit the document at the same time, Client1's operation is received by the server
      * before client2's operation and client2 is awaiting with buffer already
      */
    "Conflict resolution test 2" - {
      val server = Server("")
      val client1 = Client("", 0)
      val client2 = Client("", 0)

      val op1 = Operation().insert("World!")
      val op2 = Operation().insert("Hello ")
      val op3 = Operation().skip(6).insert("Foo ")

      val send1 = client1.applyLocal(op1)
      assert(send1.send.isDefined && send1.apply.isEmpty)
      assertMatch(client1.getState()) { case _: AwaitConfirm => }

      /** as far as we know that's the first revision that exists */
      assert(send1.send.get.revision == 1)


      val send2 = client2.applyLocal(op2)
      assert(send2.send.isDefined && send2.apply.isEmpty)
      assertMatch(client2.getState()) { case _: AwaitConfirm => }
      assert(send2.send.get.revision == 1)

      val send3 = client2.applyLocal(op3)
      assert(send3.send.isEmpty && send3.apply.isEmpty)
      assertMatch(client2.getState()) { case _: AwaitWithBuffer => }


      val response1 = server.receiveOperation(send1.send.get)
      assert(response1.isDefined && response1.get.id == send1.send.get.id && response1.get.revision == send1.send.get.revision)
      val applyRes1 = client1.applyRemote(response1.get)
      assert(applyRes1.send.isEmpty, applyRes1.apply.isEmpty)

      val applyRes2 = client2.applyRemote(response1.get)
      assert(applyRes2.send.isEmpty, applyRes2.apply.isDefined)

      val response2 = server.receiveOperation(send2.send.get)
      println(response2.get)
      assert(response2.isDefined)
      val applyRes3 = client1.applyRemote(response2.get)
      assertMatch(client1.getState()) { case _: Synchronized => }
      assert(applyRes3.send.isEmpty, applyRes3.apply.isDefined)

      val applyRes4 = client2.applyRemote(response2.get)
      assertMatch(client2.getState()) { case _: AwaitConfirm => }
      assert(applyRes4.send.isDefined, applyRes4.apply.isEmpty)

      val response5 = server.receiveOperation(applyRes4.send.get)
      assert(response5.isDefined)
      val applyres5 = client1.applyRemote(response5.get)
      assert(applyres5.send.isEmpty)
      assertMatch(client1.getState()) { case _: Synchronized => }

      val applyres6 = client2.applyRemote(response5.get)
      assert(applyres6.send.isEmpty)
      assertMatch(client2.getState()) { case _: Synchronized => }
      println(client1.str)
      assert(client1.str == client2.str, client1.str == server.str)
      assert(client1.revision==3, client2.revision==3, server.operations.length==3)
    }
  }

  def main(args: Array[String]) {
    println(new DefaultFormatter().format(test.run()))
  }
}

