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
package org.abora.white.hash;

public class FHash {
	//TODO which package should this class really be in?

	private FHash() {
	}

	/**
	 * Multiply the argument integer by a couple of enormous primes to smear it.
	 */
	public static int hashInt(int value) {
		/* 325221869 = 94349 * 88801 & 29 bits */
		return value * 325221869;
	}

	public static int hashDouble(double value) {
		if (value == 0.0 || Double.isNaN(value)) {
			return 0;
		}
		//TODO assume the Double -> long operation is fast?
		long bits = Double.doubleToLongBits(value);
		int bitsInt = (int)(bits ^ (bits >>> 32));
		return hashInt(bitsInt);
	}

}
