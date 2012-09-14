package wormhole

import java.net.Socket

class SocketInfoData(val socket:Socket) {

	val in = socket.getInputStream()
	val out = new ThreadsafeMessageWriter(socket.getOutputStream())
}