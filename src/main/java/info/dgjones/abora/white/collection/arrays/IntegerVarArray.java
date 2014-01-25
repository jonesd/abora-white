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
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class IntegerVarArray extends PrimIntegerArray {
	private final IntegerValue[] storage;


	//////////////////////////////////////////////
	// Constructors

	protected IntegerVarArray(int count) {
		super();
		storage = new IntegerValue[count];
		zeroElements();
	}

	protected IntegerVarArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		//ALREADY zeroElements (0, destOffset);
		copyElements(destOffset, from, sourceOffset, n);
		//ALREADY zeroElements (destOffset + count, size - destOffset - count);
	}

	protected IntegerVarArray(IntegerValue[] source) {
		this(source.length);
		//TODO inefficiency of zeroing array here
		System.arraycopy(source, 0, storage, 0, source.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an IntegerVarArray filled with zeros */
	public static IntegerVarArray make(int count) {
		return new IntegerVarArray(count);
	}

	/** create an IntegerVarArray filled with the indicated data in 'from' */
	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new IntegerVarArray(size, from, sourceOffset, count, destOffset);
	}

	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IntegerVarArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	public static IntegerVarArray make(PrimArray from) {
		return make(from.count(), from);
	}

	/** create an IntegerVarArray filled with the data at 'buffer' */
	public static IntegerVarArray make(IntegerValue[] buffer) {
		return new IntegerVarArray(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an actual integer value */
	public void storeIntegerVar(int index, IntegerValue value) {
		storage[index] = value;
	}

	/** Get an actual integer value */
	public IntegerValue integerVarAt(int index) {
		return storage[index];
	}

	public void storeInteger(int index, IntegerValue value) {
		storeIntegerVar(index, value);
	}

	public IntegerValue integerAt(int index) {
		return integerVarAt(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeIntegerVar(index, (IntegerValue) value);
	}

	public Heaper fetchValue(int index) {
		return integerVarAt(index);
	}

	public PrimSpec spec() {
		return PrimSpec.integerVar();
	}

	public int count() {
		return storage.length;
	}

	//////////////////////////////////////////////
	// Bulk Storage

	public void copyToBuffer(IntegerValue[] buffer, int count, int start) {
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

	protected void copyElements(int to, PrimArray source, int from, int count) {
		int n = count;
		if (n == -1) {
			n = source.count() - from;
		}
		PrimIntegerArray s = (PrimIntegerArray) source;
		for (int i = 0; i < n; i += 1) {
			storeIntegerVar(to + i, s.integerAt(from + i));
		}
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof IntegerVarArray) {
			IntegerVarArray o = (IntegerVarArray) other;
			for (int i = 0; i < count; i += 1) {
				IntegerValue a = integerVarAt(start + i);
				IntegerValue b = o.integerVarAt(otherStart + i);
				int comparison = a.compareTo(b);
				if (comparison != 0) {
					return comparison;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int index) {
		for (int i = index; i < count(); i += 1) {
			IntegerValue val = integerVarAt(i);
			int comparison = val.compareTo(IntegerValue.zero());
			if (comparison != 0) {
				return comparison;
			}
		}
		return 0;
	}

	//////////////////////////////////////////////
	// Arthmetic Operations

	protected void addData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof IntegerVarArray) {
			IntegerVarArray o = (IntegerVarArray) other;
			for (int i = 0; i < count; i += 1) {
				storeIntegerVar(i + start, integerVarAt(i + start).plus(o.integerVarAt(i + otherStart)));
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof IntegerVarArray) {
			IntegerVarArray o = (IntegerVarArray) other;
			for (int i = 0; i < count; i += 1) {
				storeIntegerVar(i + start, integerVarAt(i + start).minus(o.integerVarAt(i + otherStart)));
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(integerVarAt(index));
	}
}
