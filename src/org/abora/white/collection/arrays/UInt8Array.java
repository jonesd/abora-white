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

import java.io.PrintWriter;

import org.abora.white.value.IntegerValue;
import org.abora.white.value.PrimSpec;
import org.abora.white.xpp.basic.Heaper;

public class UInt8Array extends PrimIntArray {
	//TODO isnt a java byte actually Int8?
	private byte[] storage;


	//////////////////////////////////////////////
	// Constructors
	
	protected UInt8Array(int count) {
		super();
		storage = new byte[count];
	}

	protected UInt8Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected UInt8Array(short[] buffer) {
		this(buffer.length);
		for (int i = 0; i < buffer.length; i++) {
			short s = buffer[i];
			storage[i] = toSignedByte(s);
		}
	}
	

	////////////////////////////////////////////////////////////////////////////
	// Unsigned Util
	
	private short toUnsignedByte(byte b) {
		return (short)(b & 0xff);
	}
	
	private byte toSignedByte(short s) {
		return (byte)(s & 0xff);
	}
	
	
	//////////////////////////////////////////////
	// Static Factory Methods

	/** create a UInt8Array filled with zeros */
	public static UInt8Array make(int count) {
		return new UInt8Array(count);
	}

	/** create a UInt8Array filled with the indicated data in 'from' */
	public static UInt8Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new UInt8Array(size, from, sourceOffset, count, destOffset);
	}

	public static UInt8Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static UInt8Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static UInt8Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create a UInt8Array filled with the data at 'buffer' */
	public static UInt8Array make(short[] buffer) {
		return new UInt8Array(buffer);
	}

	/**
	 * create a UInt8Array of size strlen(string) filled with the contents of
	 * the string (keep the '\0' ?)
	 */
	public static UInt8Array string(String string) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}


	//////////////////////////////////////////////
	// Accessing

	public void storeUInt8(int index, short value) {
		throw new UnsupportedOperationException();
	}

	public short uInt8At(int index) {
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

	public PrimSpec spec() {
		return PrimSpec.uInt8();
	}

	public int bitCount() {
		return 8;
	}

	public int count() {
		return storage.length;
	}


	//////////////////////////////////////////////
	// Bulk Storage

	public void storeMany(int to, PrimArray other, int count, int from) {
		throw new UnsupportedOperationException();
	}
	
	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected void copyElements(int to, PrimArray source, int from, int count) {
		throw new UnsupportedOperationException();
	}

	//////////////////////////////////////////////
	// Printing

	public void printOn(PrintWriter oo) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, PrintWriter oo) {
		throw new UnsupportedOperationException();
	}

//	/**	
//	 * A pointer to the actual string.  While one of these are outstanding, one
//	 * may not allocate any PrimArrays, because doing so may cause compaction,
//	 * which would relocate the data.  In order to keep track of whether there
//	 * are outstanding hard pointers, my clients must call noMoreGuts() when
//	 * they will no longer be using the pointer.
//	 */
//	public String gutsOf() {
//		throw new UnsupportedOperationException();
//	}
//	public void noMoreGuts() {
//		throw new UnsupportedOperationException();
//	}


	//////////////////////////////////////////////
	// Comparing and Hashing

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
}
