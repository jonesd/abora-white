/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * Id:
 */
package org.abora.white.x;

import org.abora.white.xpp.basic.Heaper;

public class PrimIntValue extends PrimValue {
	private final int value;
	
	private static final PrimIntValue TheZero = new PrimIntValue(0);

	public PrimIntValue(int value) {
		super();
		this.value = value;
	}
	
	public static PrimIntValue zero() {
		return TheZero;
	}

	//TODO remove this?
	public boolean isEqual(Heaper other) {
		throw new UnsupportedOperationException();
	}
	
	public int intValue() {
		return value;
	}

}
