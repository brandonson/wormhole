package wormhole

import java.net.Socket

/**
 * Represents data for a socket.  Instances of this class are used to allow storage of ThreadsafeMessageWriters
 * to avoid creating more than one ThreadsafeMessageWriter per socket.  Early versions of Wormhole did not use this
 * class, and instead had different socket handler classes creating their own instances of ThreadsafeMessageWriter.
 * This caused concurrency issues with writing to the socket, which using this class fixes.
 */
class SocketInfoData(val socket:Socket) {

	/**
	 * The input stream from the socket.
	 */
	val in = socket.getInputStream()
	/**
	 * The ThreadsafeMessageWriter which writes to the socket.
	 */
	val out = new ThreadsafeMessageWriter(socket.getOutputStream())
}