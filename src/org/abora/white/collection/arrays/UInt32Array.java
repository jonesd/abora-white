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

public class UInt32Array extends PrimIntArray {

//	protected UInt32Array (Int32 count, TCSJ);

	protected UInt32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected UInt32Array(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create a UInt32Array filled with zeros */
	public static UInt32Array make(int count) {
		throw new UnsupportedOperationException();
	}

	/** create a UInt32Array filled with the indicated data in 'from' */
	public static UInt32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static UInt32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static UInt32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static UInt32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create a UInt32Array filled with the data at 'buffer' */
	public static UInt32Array make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** Store a 32 bit unsigned integer value */
	public void storeUInt(int index, int value) {
		throw new UnsupportedOperationException();
	}

	/** Get a 32 bit unsigned actual integer value */
	public int uIntAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeInteger(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

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

	private void receiveInt32Array(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	private void sendInt32Array(Xmtr xmtr) {
		throw new UnsupportedOperationException();
	}


	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, Stream oo) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

}
