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
import org.abora.white.value.PrimSpec;
import org.abora.white.xpp.basic.Heaper;

public class UInt32Array extends PrimIntArray {
	private int[] storage;


	//////////////////////////////////////////////
	// Constructors

	protected UInt32Array(int count) {
		super();
		this.storage = new int[count];
	}
	
	protected UInt32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected UInt32Array(long[] buffer) {
		this(buffer.length);
		throw new UnsupportedOperationException();
	}


	//////////////////////////////////////////////
	// Static Factory Methods
	
	/** create a UInt32Array filled with zeros */
	public static UInt32Array make(int count) {
		return new UInt32Array(count);
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
	public static UInt32Array make(long[] buffer) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}


	//////////////////////////////////////////////
	// Accessing

	/** Store a 32 bit unsigned integer value */
	public void storeUInt(int index, int value) {
		throw new UnsupportedOperationException();
	}

	/** Get a 32 bit unsigned actual integer value */
	public int uIntAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeInteger(int index, IntegerValue value) {
		throw new UnsupportedOperationException();
	}

	public IntegerValue integerAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeValue(int index, Heaper value) {
		throw new UnsupportedOperationException();
	}

	public Heaper fetchValue(int index) {
		throw new UnsupportedOperationException();
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		return PrimSpec.uInt32();
	}

	public int bitCount() {
		/* Return the maximum bits/entry that can be stored in this array.
		   The number will be negative for signed arrays. */

		return 32;
	}


	//////////////////////////////////////////////
	// Bulk Storing
	
	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}


	//////////////////////////////////////////////
	// Comparison and Hashing
	
	protected int compareData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}


	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}


	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintStream oo) {
		throw new UnsupportedOperationException();
	}
}
