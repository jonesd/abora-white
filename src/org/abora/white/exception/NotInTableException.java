package org.abora.white.exception;

public class NotInTableException extends AboraRuntimeException {

	public NotInTableException() {
		super();
	}

	public NotInTableException(String message) {
		super(message);
	}

	public NotInTableException(String message, Throwable cause) {
		super(message, cause);
	}

	public NotInTableException(Throwable cause) {
		super(cause);
	}

}
