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

/**
 * Boxed Java double or IEEE64 value. 
 */
public class IEEE64Value extends PrimFloatValue {
	private double value;
	
	protected IEEE64Value(double value) {
		super();
		this.value = value;
	}

	public static IEEE64Value make(double value) {
		return new IEEE64Value(value);
	}

	public float asIEEE32() {
		return (float)value;
	}

	public double asIEEE64() {
		return value;
	}

}
