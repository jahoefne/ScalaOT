package scalot

import org.http4s._
import org.http4s.server._
import org.http4s.dsl._
import org.http4s.argonaut._
import org.http4s.server.blaze.BlazeBuilder
import scalatags.Text.all._

object Page{
  def render() = html(
    head(
      script(
        "alert('Hello World')"
      )
    ),
    body(
      div(
        h1(id:="title", "This is a title"),
        p("This is a big paragraph of text")
      )
    )
  ).toString()
}



object Main {

  object Example {
    val service = HttpService {

      case GET -> Root =>
        Ok(Page.render())
    }
  }

  def main(args: Array[String]) {
    BlazeBuilder.bindHttp(8080)
      .mountService(Example.service, "/")
      .run
      .awaitShutdown()

  }
}

    /*
    val server = ServerDocument("")
    val client = Client("",0)
    val client2 = Client("",0)

    val op = Operation().insert("Hello World!")
    val op2 = Operation().skip(6).insert("Cruel ").skip(6)



    def handleServer(op: Option[Operation]) : Unit = {
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
    println(s"Client2 ${client2.str}")*/
