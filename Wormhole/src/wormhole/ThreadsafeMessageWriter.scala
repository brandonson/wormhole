package wormhole;

import java.io.IOException
import java.io.OutputStream
import java.util.concurrent.LinkedBlockingQueue
import java.util.concurrent.TimeUnit
import com.google.protobuf.Message
import scala.collection.JavaConversions
import akka.actor.Props
import akka.actor.Actor
import wormhole.game.network.GameProto
import java.util.Arrays
class ThreadsafeMessageWriter(output:OutputStream, delimit:Boolean = true){
	val ref = WormholeSystem.actorOf(Props(new ThreadsafeMessageWriterImpl(output, delimit)))
	
	def write(messages:Message*){
		println("WRITE=============================")
		println(Thread.currentThread().getStackTrace() foreach {st => println(st.getMethodName() + ":" + st.getClassName() +":" + st.getLineNumber())})
		val msgList = messages.toList
		write(msgList)
	}
	def write(messages:List[Message]){
		ref ! messages
	}
	def close(){
		ref ! 'Close
	}
}

class ThreadsafeMessageWriterImpl(val output:OutputStream, val delimit:Boolean) extends Actor{
	
	def receive = {
		case messages:List[Message] =>
			try{
				messages foreach {_.writeDelimitedTo(output)}
			}catch{
				case _:IOException =>
					context.stop(self)
			}
		case 'Close =>
			output.close()
			context.stop(self)
	}
}
