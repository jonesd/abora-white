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
package org.abora.white.collection.arrays;

public abstract class PrimIntArray extends PrimIntegerArray {

	protected PrimIntArray() {
		super();
	}

	/**	
	 * Make an array initialized to zeros. The values are signed if bitCount is
	 * negative
	 */
	public static PrimIntArray zeros(int bitCount, int count) {
		throw new UnsupportedOperationException();
	}

	//	public int bitCount() {
	//		throw new UnsupportedOperationException();
	//	}
}
