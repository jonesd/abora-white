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

/**
 * Boxed Java float or IEEE32 value. 
 */
public class IEEE32Value extends PrimFloatValue {

	private float value;

	protected IEEE32Value(float value) {
		super();
		this.value = value;
	}

	public static Heaper make(float f) {
		return new IEEE32Value(f);
	}

	/**
	 * The value as an IEEE 32-bit floating point number
	 */
	public float asIEEE32() {
		return value;
		/*
		all.st:36858:PrimIEEE32 methodsFor: 'accessing'!
		{IEEE32} asIEEE32
			"The value as an IEEE 32-bit floating point number"
			^ myValue!
		*/
	}

	public double asIEEE64() {
		return value;
	}
}
