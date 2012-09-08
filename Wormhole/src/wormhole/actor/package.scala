package wormhole

import akka.util.Timeout
import akka.util.duration._
import java.util.concurrent.TimeUnit
import akka.dispatch.Await
import akka.dispatch.Future
package object actor {
	implicit val timeout = new Timeout(5, TimeUnit.SECONDS)
	implicit val duration = 5 seconds
	
	def fetch[T](future:Future[T]):T = Await.result(future, duration)
}