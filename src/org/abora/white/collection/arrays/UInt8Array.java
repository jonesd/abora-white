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
import org.abora.white.value.PrimIntegerSpec;
import org.abora.white.value.PrimSpec;
import org.abora.white.xpp.basic.Heaper;

public class UInt8Array extends PrimIntArray {
	private final byte[] storage;


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
		storage[index] = toSignedByte(value);
	}

	public short uInt8At(int index) {
		return toUnsignedByte(storage[index]);
	}

	public void storeInteger(int index, IntegerValue value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeUInt8(index, value.asUInt8());
	}

	public IntegerValue integerAt(int index) {
		return IntegerValue.make(uInt8At(index));
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		IntegerValue v = (IntegerValue) value;
		storeInteger(index, v);
	}

	public Heaper fetchValue(int index) {
		return IntegerValue.make(uInt8At(index));
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

//	public void storeMany(int to, PrimArray other, int count, int from) {
//		throw new UnsupportedOperationException();
//	}
	
	public void copyToBuffer(short[] buffer, int count, int start) {
		int n;
		if (count >= 0) {
			n = count;
		} else {
			n = count() - start;
		}
		if (n > buffer.length) {
			n = buffer.length;
		}
		for (int i = 0; i < n; i++) {
			short s = uInt8At(start + i);
			buffer[i] = s;
		}
	}

//	public void zeroElements(int from, int count) {
//		throw new UnsupportedOperationException();
//	}

//	protected void copyElements(int to, PrimArray source, int from, int count) {
//		throw new UnsupportedOperationException();
//	}


	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof UInt8Array) {
			UInt8Array o = (UInt8Array) other;
			for (int i = 0; i < count; i += 1) {
				int cmp = uInt8At(i + start) - o.uInt8At(i + otherStart);
				if (cmp != 0) {
					return cmp < 0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			short value = uInt8At(i);
			if (value < 0) {
				return -1;
			}
			if (value > 0) {
				return +1;
			}
		}
		return 0;
	}


	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof UInt8Array) {
			UInt8Array o = (UInt8Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = uInt8At(i + start) + o.uInt8At(i + otherStart);
				storeUInt8(i + start, (short) resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof UInt8Array) {
			UInt8Array o = (UInt8Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = uInt8At(i + start) - o.uInt8At(i + otherStart);
				storeUInt8(i + start, (short) resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}


	//////////////////////////////////////////////
	// Printing

//	public void printOn(PrintWriter oo) {
//		throw new UnsupportedOperationException();
//	}

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(uInt8At(index));
	}

	//////////////////////////////////////////////
	// Conversions
	
	public String asString() {
		throw new UnsupportedOperationException();
	}
}
