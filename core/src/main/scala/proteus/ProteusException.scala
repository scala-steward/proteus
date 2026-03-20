package proteus

/**
  * Base exception type for errors raised by the Proteus library.
  */
class ProteusException(message: String, cause: Throwable = null) extends RuntimeException(message, cause)
