/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.white.exception;

public class AboraRuntimeException extends RuntimeException {

	public AboraRuntimeException() {
		super();
	}

	public AboraRuntimeException(String message) {
		super(message);
	}

	public AboraRuntimeException(String message, Throwable cause) {
		super(message, cause);
	}

	public AboraRuntimeException(Throwable cause) {
		super(cause);
	}

}
