package org.abora.white.exception;

public class CopyOutOfBoundsException extends AboraRuntimeException {

	public CopyOutOfBoundsException() {
		super();
	}

	public CopyOutOfBoundsException(String message) {
		super(message);
	}

	public CopyOutOfBoundsException(String message, Throwable cause) {
		super(message, cause);
	}

	public CopyOutOfBoundsException(Throwable cause) {
		super(cause);
	}

}
