/*
 * Abora hypertext system
 * Copyright 2003 David G Jones, david_jones@night.dircon.co.uk
 * 
 * Translated from Udanax-Gold source code: Copyright 1991 XOC, www.udanax.com
 */

package org.abora.white.collection.basic;

import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

/**
 * @author jonesd
 */
public class PrimIntArray extends PrimIntegerArray {

		protected PrimIntArray(int count, int datumSize) {
			super(count, datumSize);
		}
	
	/**	
	 * Make an array initialized to zeros. The values are signed if bitCount is
	 * negative
	 */ 
	public static PrimIntArray zeros(IntegerVar bitCount, IntegerVar count) {
		throw new UnsupportedOperationException();
	}

	/** Store an integer value */
	public void storeInteger(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual integer value */
	public IntegerVar integerAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeValue(int index, Heaper value) {
		throw new UnsupportedOperationException();
	}

	public Heaper fetchValue(int index) {
		throw new UnsupportedOperationException();
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
	}

	public int bitCount() {
		throw new UnsupportedOperationException();
	}
 
	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, Stream oo) {
		throw new UnsupportedOperationException();
	}

	public PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}
}
