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
