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
package org.abora.white.value;

import org.abora.white.xpp.basic.Heaper;

public class IntegerValue extends PrimIntValue {
	//TODO implement overflow to BigInteger
	private final int value;
	
	private static final IntegerValue TheZero = new IntegerValue(0);

	public IntegerValue(int value) {
		super();
		this.value = value;
	}
	
	public static IntegerValue zero() {
		return TheZero;
	}

	public static IntegerValue make(int value) {
		return new IntegerValue(value);
	}

	//TODO remove this?
	public boolean isEqual(Heaper other) {
		throw new UnsupportedOperationException();
	}
	
	public int intValue() {
		return value;
	}

}
