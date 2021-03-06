/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.white.collection.arrays;

import java.io.PrintWriter;

import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.value.PrimIntegerSpec;
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Concrete fixed size array that holds elements of the 16-bit signed integral type.
 * This maps to the Java <code>short</int> primitive type.
 */

public class Int64Array extends PrimIntArray {
	private final long[] storage;

	//////////////////////////////////////////////
	// Constructors

	protected Int64Array(int count) {
		super();
		storage = new long[count];
	}

	protected Int64Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected Int64Array(long[] buffer) {
		this(buffer.length);
		System.arraycopy(buffer, 0, storage, 0, buffer.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an Int64Array filled with zeros */
	public static Int64Array make(int count) {
		return new Int64Array(count);
	}

	/** create an Int64Array filled with the indicated data in 'from' */
	public static Int64Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new Int64Array(size, from, sourceOffset, count, destOffset);
	}

	public static Int64Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int64Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int64Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static Int64Array make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an Int64Array filled with the data at 'buffer' */
	public static Int64Array make(long[] buffer) {
		return new Int64Array(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an 64 bit signed integer value */
	public void storeInt64(int index, long value) {
		storage[index] = value;
	}

	/** Get an 64 bit signed actual integer value */
	public long int64At(int index) {
		return storage[index];
	}

	public void storeInteger(int index, IntegerValue value) {
		if (!((PrimIntegerSpec) spec()).canHold(value)) {
			throw new IllegalArgumentException("ValueOutOfRange");
		}
		storeInt64(index, value.asInt64());
	}

	public IntegerValue integerAt(int index) {
		return IntegerValue.make(int64At(index));
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeInteger(index, (IntegerValue) value);
	}

	public Heaper fetchValue(int index) {
		return IntegerValue.make(int64At(index));
	}

	public int count() {
		return storage.length;
	}

	public PrimSpec spec() {
		return PrimSpec.int64();
	}

	public int bitCount() {
		return -64;
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
	public void copyToBuffer(long[] buffer, int count, int start) {
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
		if (other instanceof Int64Array) {
			Int64Array o = (Int64Array) other;
			for (int i = 0; i < count; i += 1) {
				long cmp1 = int64At(i + start);
				long cmp2 = o.int64At(i + otherStart);
				if (cmp1 < cmp2) {
					return -1;
				} else if (cmp1 > cmp2) {
					return +1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			long val = int64At(i);
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
		if (other instanceof Int64Array) {
			Int64Array o = (Int64Array) other;
			for (int i = 0; i < count; i += 1) {
				long resultant = int64At(i + start) + o.int64At(i + otherStart);
				storeInt64(i + start, resultant);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof Int64Array) {
			Int64Array o = (Int64Array) other;
			for (int i = 0; i < count; i += 1) {
				long resultant = int64At(i + start) - o.int64At(i + otherStart);
				storeInt64(i + start, resultant);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(int64At(index));
	}
}
