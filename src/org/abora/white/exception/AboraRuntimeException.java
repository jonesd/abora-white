package org.abora.white.exception;

/**
 * @author jonesd
 */
public class AboraRuntimeException extends RuntimeException {

	/**
	 * Constructor for AboraRuntimeException.
	 */
	public AboraRuntimeException() {
		super();
	}

	/**
	 * Constructor for AboraRuntimeException.
	 * @param message
	 */
	public AboraRuntimeException(String message) {
		super(message);
	}

	/**
	 * Constructor for AboraRuntimeException.
	 * @param message
	 * @param cause
	 */
	public AboraRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}

	/**
	 * Constructor for AboraRuntimeException.
	 * @param cause
	 */
	public AboraRuntimeException(Throwable cause) {
		super(cause);
	}

}
