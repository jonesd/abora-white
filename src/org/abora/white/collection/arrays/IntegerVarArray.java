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

public class IntegerVarArray extends PrimIntegerArray {
	private final IntegerValue[] storage;


	//////////////////////////////////////////////
	// Constructors

	protected IntegerVarArray(int count) {
		super();
		storage = new IntegerValue[count];
	}

	protected IntegerVarArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	protected IntegerVarArray(IntegerValue[] source) {
		this(source.length);
		System.arraycopy(source, 0, storage, 0, source.length);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create an IntegerVarArray filled with zeros */
	public static IntegerVarArray zeros(int count) {
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

	/** create an IntegerVarArray filled with the data at 'buffer' */
	public static IntegerVarArray make(IntegerValue[] buffer) {
		return new IntegerVarArray(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimIntegerArray) source, sourceOffset, count, destOffset);
		//		RPTR(PrimArray) IntegerVarArray::makeNew (Int32 size,
		//							  APTR(PrimArray) source,
		//							  Int32 sourceOffset,
		//							  Int32 count,
		//							  Int32 destOffset)
		//		{
		//			return IntegerVarArray::make (size, CAST(PrimIntegerArray,source),
		//						  sourceOffset, count, destOffset);
		//		}
	}

	//////////////////////////////////////////////
	// Accessing

	/** Store an actual integer value */
	public void storeIntegerVar(int index, IntegerValue value) {
		storage[index] = value;
		//		INLINE void IntegerVarArray::storeIntegerVar (Int32 index, IntegerVar value){
		//			/* Store an integer value */
		//			((IntegerVar*)this->storage())[this->rangeCheck (index)] = value;
		//		}
	}

	/** Get an actual integer value */
	public IntegerValue integerVarAt(int index) {
		return storage[index];
		//		INLINE IntegerVar IntegerVarArray::integerVarAt (Int32 index){
		//			/* Get an actual integer value */
		//			return ((IntegerVar*)this->storage())[this->rangeCheck (index)];
		//		}
	}

	public void storeInteger(int index, IntegerValue value) {
		storeIntegerVar(index, value);

		//		void IntegerVarArray::storeInteger (Int32 index, IntegerVar value){
		//			/* Store an integer value */
		//
		//			this->storeIntegerVar(index, value);
		//		}
	}

	public IntegerValue integerAt(int index) {
		return integerVarAt(index);
		//		IntegerVar IntegerVarArray::integerAt (Int32 index){
		//			/* Get an actual integer value */
		//
		//			return this->integerVarAt(index);
		//		}
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeIntegerVar(index, (IntegerValue) value);
		//		void IntegerVarArray::storeValue (Int32 index, APTR(Heaper) OR(NULL) value){
		//			if (value == NULL) {
		//			BLAST(NULL_VALUE);
		//			}
		//			this->storeIntegerVar(index, CAST(PrimIntValue,value)->asIntegerVar());
		//		}
	}

	public Heaper fetchValue(int index) {
		return integerVarAt(index);
		//		RPTR(Heaper) OR(NULL) IntegerVarArray::fetchValue (Int32 index) {
		//			return PrimIntValue::make(this->integerVarAt(index));
		//		}
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

		//		void IntegerVarArray::copyToBuffer (void * buffer,
		//							Int32 size,
		//							Int32 count /*= -1*/,
		//							Int32 start /* = Int32Zero*/)
		//		{
		//			Int32 bufSize;
		//			Int32 n;
		//
		//			bufSize = size / sizeof(IntegerVar);
		//			if (count >= 0) {
		//			n = count;
		//			} else {
		//			n = this->count() - start;
		//			}
		//			if (n > bufSize) {
		//			n = bufSize;
		//			}
		//			IntegerVar * source = (IntegerVar*)this->storage();
		//			for (Int32 i = 0; i < n; i += 1) {
		//			((IntegerVar*)buffer)[i] = source[i + start];
		//			}
		//		}
	}

	public void zeroElements(int from, int count) {
		int n = count;
		if (n < 0) {
			n = count();
		}
		if (n + from > count()) {
			throw new IllegalArgumentException("TooManyZeros");
		}
		if (from < 0) {
			throw new IndexOutOfBoundsException("BogusStartIndex");
		}
		for (int i = 0; i < n; i += 1) {
			storeIntegerVar(from + i, IntegerValue.zero());
		}
		//		void IntegerVarArray::zeroElements (Int32 from, Int32 count)
		//		{
		//			Int32 n = count;
		//			if (n < 0) {
		//			n = this->count();
		//			}
		//			if (n+from > this->count()) {
		//			BLAST(TOO_MANY_ZEROS);
		//			}
		//			if (from < 0) {
		//			BLAST(BogusStartIndex);
		//			}
		//			for (Int32 i = 0; i < n; i += 1) {
		//			this->storeIntegerVar (from + i, IntegerVarZero);
		//			}
		//		}
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
		//		void IntegerVarArray::copyElements (Int32 to, 
		//							APTR(PrimArray) source,
		//							Int32 from,
		//							Int32 count)
		//		{
		//			Int32 n = count;
		//			if (n == -1) {
		//			n = source->count() - from;
		//			}
		//			SPTR(PrimIntegerArray) s = CAST(PrimIntegerArray,source);
		//			for (Int32 i = 0; i < n; i += 1) {
		//			this->storeIntegerVar (to + i, s->integerAt (from + i));
		//			}
		//		}
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

		//		Int32 IntegerVarArray::compareData (Int32 start, 
		//							APTR(PrimDataArray) other,
		//							Int32 otherStart,
		//							Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IntegerVarArray,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				IntegerVar a = this->integerVarAt(start + i);
		//				IntegerVar b = o->integerVarAt(otherStart + i);
		//				if (a < b) {
		//					return -1;
		//				}
		//				if (a > b) {
		//					return 1;
		//				}
		//				}
		//				return 0;
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				return this->PrimIntegerArray::compareData (start, other,
		//									otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//			return 0;
		//		}
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
		//		Int32 IntegerVarArray::signOfNonZeroAfter (Int32 index) {
		//			for (Int32 i = index; i < this->count(); i += 1) {
		//			IntegerVar val = this->integerVarAt(i);
		//			if (val < IntegerVarZero) {
		//				return -1;
		//			}
		//			if (val > IntegerVarZero) {
		//				return +1;
		//			}
		//			}
		//			return 0;
		//		}
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
		//		void IntegerVarArray::addData (Int32 start, 
		//						   APTR(PrimDataArray) other,
		//						   Int32 otherStart,
		//						   Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IntegerVarArray,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeIntegerVar (i + start,
		//							   this->integerVarAt(i + start) 
		//							   + o->integerVarAt(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimIntegerArray::addData (start, other, otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
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
		//		void IntegerVarArray::subtractData (Int32 start, 
		//							APTR(PrimDataArray) other,
		//							Int32 otherStart,
		//							Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IntegerVarArray,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeIntegerVar (i + start,
		//							   this->integerVarAt(i + start) 
		//							   - o->integerVarAt(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimIntegerArray::subtractData (start, other, otherStart,
		//								  count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}

	//////////////////////////////////////////////
	// Printing

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(integerVarAt(index));
		//		void IntegerVarArray::printElementOn (Int32 index, ostream& oo)
		//		{
		//			oo << this->integerVarAt (index);
		//		}
	}
}
