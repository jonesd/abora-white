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

import java.io.PrintStream;

import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class Int32Array extends PrimIntArray {
	private int[] storage;

	protected Int32Array (int count) {
		super();
		storage = new int[count];
	}

	protected Int32Array(PrimArray from, int sourceOffset, int count, int destOffset) {
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		//TODO try and allocate storage from one place
		storage = new int[n];
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int32Array(int[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	/** create an Int32Array filled with zeros */
	public static Int32Array make(int count) {
		return new Int32Array(count);
	}

	/** create an Int32Array filled with the indicated data in 'from' */
	public static Int32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static Int32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create an Int32Array filled with the data at 'buffer' */
	public static Int32Array make(int[] buffer) {
		return new Int32Array(buffer);
	}


	//////////////////////////////////////////////
	// accessing
	
	/** Store a 32 bit signed integer value */
	public void storeInt(int index, int value) {
		throw new UnsupportedOperationException();
	}

	/** Get a 32 bit signed actual integer value */
	public int intAt(int index) {
		return storage[index];
	}

	public void storeInteger(int index, IntegerValue value) {
		throw new UnsupportedOperationException();
	}

	public IntegerValue integerAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeInt(index, ((IntegerValue) value).asInt32());
	}

	public Heaper fetchValue(int index) {
		return IntegerValue.make(intAt(index));
	}

	//	  public PrimSpec spec() {
	//	  	throw new UnsupportedOperationException();
	//	  }

	//	  public int bitCount() {
	//	  	throw new UnsupportedOperationException();
	//	  }

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	protected int compareData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	protected void addData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, PrintStream oo) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public int count() {
		return storage.length;
	}

}
