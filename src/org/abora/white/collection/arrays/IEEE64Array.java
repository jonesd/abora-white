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
import java.util.Arrays;

import org.abora.white.x.PrimFloatValue;
import org.abora.white.x.PrimIEEE64;
import org.abora.white.xpp.basic.Heaper;

/**
 * Basic array composed of Java double or IEEE64 values.
 */
public class IEEE64Array extends PrimFloatArray {
	private double[] storage;

	protected IEEE64Array(int count) {
		super();
		storage = new double[count];
	}

	protected IEEE64Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected IEEE64Array(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create an IEEE64 array filled with zeros */
	public static IEEE64Array make(int count) {
		throw new UnsupportedOperationException();
	}

	/** create an IEEE64Array filled with the indicated data in 'from' */
	public static IEEE64Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static IEEE64Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IEEE64Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IEEE64Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create an IEEE64Array filled with the data at 'buffer' */
	public static IEEE64Array make(int count, double[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** Store an actual floating point value */
	public void storeIEEE64(int index, double value) {
		storage[rangeCheck(index)] = value;
	}

	/** Get an actual floating point number */
	public double iEEE64At(int index) {
		return storage[rangeCheck(index)];
	}

	public void storeFloat(int index, double value) {
		storeIEEE64(index, value);
	}

	public double floatAt(int index) {
		return iEEE64At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeIEEE64(index, ((PrimFloatValue) value).asIEEE64());
	}

	public Heaper fetchValue(int index) {
		return PrimIEEE64.make(iEEE64At(index));
	}

	//	public PrimSpec spec() {
	//		throw new UnsupportedOperationException();
	//	}

	//	/** Return the maximum word size that can be stored in this array */
	//	public int bitCount() {
	//		throw new UnsupportedOperationException();
	//	}

	public void storeAll(Heaper value, int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		double f;
		if (value == null) {
			f = 0.0;
		} else {
			f = ((PrimFloatValue) value).asIEEE64();
		}
		for (int i = 0; i < n; i += 1) {
			storeIEEE64(start + i, f);
		}
		//		void IEEE64Array::storeAll (APTR(Heaper) value/* = NULL*/,
		//						Int32 count/* = -1*/,
		//						Int32 start/* = Int32Zero*/)
		//		{
		//			IEEE64 f;
		//			Int32 n;
		//
		//			n = this->count() - start;
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			if (value == NULL) {
		//			f = 0.0;
		//			} else {
		//			f = CAST(PrimFloatValue,value)->asIEEE64();
		//			}
		//			for (Int32 i = 0; i < n; i += 1) {
		//			this->storeIEEE64(start + i, f);
		//			}
		//		}
	}

	public void copyToBuffer(double[] buffer, int count, int start) {
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
		//		void IEEE64Array::copyToBuffer (void * buffer,
		//						Int32 size,
		//						Int32 count /*= -1*/,
		//						Int32 start /* = Int32Zero*/)
		//		{
		//			Int32 bufSize;
		//			Int32 n;
		//
		//			bufSize = size / sizeof(IEEE64);
		//			if (count >= 0) {
		//			n = count;
		//			} else {
		//			n = this->count() - start;
		//			}
		//			if (n > bufSize) {
		//			n = bufSize;
		//			}
		//			MEMMOVE (buffer, (IEEE64*)this->storage() + start,
		//				 (int)(n * sizeof(IEEE64)));
		//		}
	}

	public void zeroElements(int from, int count) {
		int n = count;
		if (n < 0) {
			n = count();
		}
		Arrays.fill(storage, from, from + n, 0.0);
		//		void IEEE64Array::zeroElements (Int32 from, Int32 count)
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
		//			Arrays.fill(storage, from, count, val)
		//			memset ((IEEE64*)this->storage()+from, 0, (int)(n * sizeof(IEEE64)));
		//		}
	}

	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE64Array) {
			IEEE64Array o = (IEEE64Array) other;
			for (int i = 0; i < count; i += 1) {
				double cmp = iEEE64At(i + start) - o.iEEE64At(i + otherStart);
				if (cmp != 0.0) {
					return ((int) cmp) < 0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
		//		Int32 IEEE64Array::compareData (Int32 start, 
		//						APTR(PrimDataArray) other,
		//						Int32 otherStart,
		//						Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IEEE64Array,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				IEEE64 cmp;
		//				cmp = this->iEEE64At(i + start) - o->iEEE64At(i + otherStart);
		//				if (cmp != 0.0) {
		//					return ((Int32) cmp) < 0 ? -1 : 1;
		//				}
		//				}
		//				return 0;
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				return this->PrimFloatArray::compareData (start, other,
		//									  otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//			return 0;
		//		}
	}

	protected int signOfNonZeroAfter(int start) {
		for (int i = start; i < count(); i += 1) {
			double val = iEEE64At(i);
			if (val < 0.0) {
				return -1;
			}
			if (val > 0.0) {
				return +1;
			}
		}
		return 0;
		//		Int32 IEEE64Array::signOfNonZeroAfter (Int32 index) {
		//			for (Int32 i = index; i < this->count(); i += 1) {
		//			IEEE64 val;
		//	
		//			if ((val = this->iEEE64At(i)) < 0.0) {
		//				return -1;
		//			}
		//			if (val > 0.0) {
		//				return +1;
		//			}
		//			}
		//			return 0;
		//		}
	}

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE64Array) {
			IEEE64Array o = (IEEE64Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE64(i + start, iEEE64At(i + start) + o.iEEE64At(i + otherStart));
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
		//		void IEEE64Array::addData (Int32 start, 
		//					   APTR(PrimDataArray) other,
		//					   Int32 otherStart,
		//					   Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IEEE64Array,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeIEEE64 (i + start,
		//						   this->iEEE64At(i + start) 
		//						   + o->iEEE64At(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimFloatArray::addData (start, other, otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof IEEE64Array) {
			IEEE64Array o = (IEEE64Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE64(i + start, iEEE64At(i + start) - o.iEEE64At(i + otherStart));
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
		//		void IEEE64Array::subtractData (Int32 start, 
		//						APTR(PrimDataArray) other,
		//						Int32 otherStart,
		//						Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(IEEE64Array,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeIEEE64 (i + start,
		//						   this->iEEE64At(i + start) 
		//						   - o->iEEE64At(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimFloatArray::subtractData (start, other, otherStart,
		//								count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}

	protected void printElementOn(int index, PrintStream oo) {
		oo.print(iEEE64At(index));
	}

	protected void copyElements(int to, PrimArray source, int from, int count) {
		int n = count;
		if (n == -1) {
			n = source.count() - from;
		}
		double[] sourceStorage = ((IEEE64Array) source).storage;
		System.arraycopy(sourceStorage, from, storage, to, n);
		//		void IEEE64Array::copyElements (Int32 to, 
		//						APTR(PrimArray) source,
		//						Int32 from,
		//						Int32 count)
		//		{
		//			Int32 n = count;
		//			if (n == -1) {
		//			n = source->count() - from;
		//			}
		//			/* we can hold the source storage pointer since this is atomic */
		//			IEEE64 * sourceBits = ((IEEE64*)CAST(IEEE64Array,source)->storage()) +from;
		//			IEEE64 * destBits = ((IEEE64*)this->storage()) + to;
		//			MEMMOVE (destBits, sourceBits, (int) (n * sizeof(IEEE64)));
		//		}
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public int count() {
		return storage.length;
	}
}
