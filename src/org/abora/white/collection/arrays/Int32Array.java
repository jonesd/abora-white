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

public class Int32Array extends PrimIntArray {
	private int[] storage;

	//////////////////////////////////////////////
	// Constructors

	protected Int32Array(int count) {
		super();
		storage = new int[count];
	}

	protected Int32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int32Array(int[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an Int32Array filled with zeros */
	public static Int32Array make(int count) {
		return new Int32Array(count);
	}

	/** create an Int32Array filled with the indicated data in 'from' */
	public static Int32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new Int32Array(size, from, sourceOffset, count, destOffset);
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

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store a 32 bit signed integer value */
	public void storeInt32(int index, int value) {
		storage[index] = value;
	}

	/** Get a 32 bit signed actual integer value */
	public int int32At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, IntegerValue value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeInt32(index, value.asInt32());
	}

	public IntegerValue integerAt(int index) {
		return IntegerValue.make(int32At(index));
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeInteger(index, (IntegerValue)value);
	}

	public Heaper fetchValue(int index) {
		return IntegerValue.make(int32At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		return PrimSpec.int32();
	}

	public int bitCount() {
		/* Return the maximum bit/entry that can be stored in this array.
		   The number will be negative for signed arrays. */

		return -32;
	}

	//////////////////////////////////////////////
	// Bulk Storage

	/** 
	 * Copy a consequitive range of elements from the receiver into the
	 * supplied buffer.
	 *  
	 * @param buffer array to fill with receveirs elements
	 * @param count number of consequentive elements in range or all
	 * 			elements from start if -1. Silently truncate if count is
	 * 			larger than available elements in the receiver
	 * @param start index of first element in range
	 */
	public void copyToBuffer(int[] buffer, int count, int start) {
		int n;
		if (count >= 0) {
			n = count;
		} else {
			n = count() - start;
		}
		if (n > buffer.length) {
			n = buffer.length;
		}
		System.arraycopy(storage, start, buffer, 0, n);
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof Int32Array) {
			Int32Array o = (Int32Array) other;
			for (int i = 0; i < count; i += 1) {
				int cmp = int32At(i + start) - o.int32At(i + otherStart);
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
			int val = int32At(i);
			if (val < 0) {
				return -1;
			}
			if (val > 0) {
				return +1;
			}
		}
		return 0;
	}

	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof Int32Array) {
			Int32Array o = (Int32Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int32At(i + start) + o.int32At(i + otherStart);
				storeInt32(i + start, resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof Int32Array) {
			Int32Array o = (Int32Array) other;
			for (int i = 0; i < count; i += 1) {
				int resultant = int32At(i + start) - o.int32At(i + otherStart);
				storeInt32(i + start, resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(int32At(index));
	}
}
