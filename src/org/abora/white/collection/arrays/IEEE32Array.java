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

import org.abora.white.value.PrimFloatValue;
import org.abora.white.value.IEEE32Value;
import org.abora.white.xpp.basic.Heaper;

/**
 * Array collection which holds IEEE32 values,
 * which match the Java float primitive type.
 */
public class IEEE32Array extends PrimFloatArray {
	private float[] storage = null;


	//////////////////////////////////////////////
	// Constructors

	/** 
	 * Construct a new array of the specified size with
	 * all elements initialized to zero.
	 *
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 * 
	 * @param count number of elements this will be able to hold
	 */
	protected IEEE32Array(int count) {
		super();
		storage = new float[count];
	}

	protected IEEE32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		this(size);
		int n = count;
		if (count == -1) {
			n = from.count() - sourceOffset;
		}
		copyElements(destOffset, from, sourceOffset, n);
	}

	/** 
	 * Construct a new array of the same size as the specified source
	 * and containing a copy of its content. 
	 *
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 * 
	 * @param source primitive array to copy
	 */
	protected IEEE32Array(float[] source) {
		this(source.length);
		System.arraycopy(source, 0, storage, 0, source.length);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PrimFloatArray) source, sourceOffset, count, destOffset);
	}


	//////////////////////////////////////////////
	// Static Factory Methods
	
	/** 
	 * Return a new IEEE32Array of the specified size suitable for
	 * holding IEEE32 values, initially filled with zeros.
	 *  
	 * @param count number of elements this will be able to hold
	 */
	public static IEEE32Array make(int count) {
		return new IEEE32Array(count);
	}

	/** create an IEEE32Array filled with the indicated data in 'from' */
	public static IEEE32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new IEEE32Array(size, from, sourceOffset, count, destOffset);
	}

	public static IEEE32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IEEE32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IEEE32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}
	
	/** create an IEEE32Array filled with the data at 'buffer' */
	public static IEEE32Array make(float[] buffer) {
		return new IEEE32Array(buffer);
	}


	//////////////////////////////////////////////
	// accessing

	/** Store an actual floating point value */
	public void storeIEEE32(int index, float value) {
		storage[index] = value;
	}

	/** Get an actual floating point number */
	public float iEEE32At(int index) {
		return storage[index];
	}

	public void storeFloat(int index, double value) {
		storeIEEE32(index, (float) value);
	}

	public double floatAt(int index) {
		return (double) iEEE32At(index);
	}

	public void storeValue(int index, Heaper value) {
		if (value == null) {
			throw new NullPointerException();
		}
		storeIEEE32(index, ((PrimFloatValue) value).asIEEE32());
	}

	public Heaper fetchValue(int index) {
		return IEEE32Value.make(iEEE32At(index));
	}

	//	public PrimSpec spec() {
	//		return PrimSpec.iEEE32();
	//
	//		//		RPTR(PrimSpec) IEEE32Array::spec (){
	//		//			return PrimSpec::iEEE32();
	//		//		}
	//	}

	//	/** Return the maximum word size that can be stored in this array */
	//	public int bitCount() {
	//		return 32;
	//
	//		//		Int32 IEEE32Array::bitCount () {
	//		//			/* Return the maximum bits/entry that can be stored in this array */
	//		//
	//		//			return 32;
	//		//		}
	//	}


	//////////////////////////////////////////////
	// bulk storing

	/** 
	 * Fill a consequitive range of elements with the supplied value.
	 *  
	 * @param value to store within range or 0.0 if null
	 * @param count number of consequentive elements in range or all
	 * 			elements from start if -1
	 * @param start index of first element in range (default to start)
	 */
	public void storeAll(Heaper value, int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		float f;
		if (value == null) {
			f = 0.0f;
		} else {
			f = ((PrimFloatValue) value).asIEEE32();
		}
		for (int i = 0; i < n; i += 1) {
			storeIEEE32(start + i, f);
		}
	}

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
	public void copyToBuffer(float[] buffer, int count, int start) {
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

	protected int compareData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				float cmp = iEEE32At(i + myStart) - o.iEEE32At(i + otherStart);
				if (cmp != 0.0) {
					return ((int) cmp) < 0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(myStart, other, otherStart, count);
		}
	}

	protected int signOfNonZeroAfter(int start) {
		for (int i = start; i < count(); i += 1) {
			float val = iEEE32At(i);
			if (val < 0.0) {
				return -1;
			}
			if (val > 0.0) {
				return +1;
			}
		}
		return 0;
	}

	protected void addData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE32(i + myStart, iEEE32At(i + myStart) + o.iEEE32At(i + otherStart));
			}
		} else {
			super.addData(myStart, other, otherStart, count);
		}
	}

	protected void subtractData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof IEEE32Array) {
			IEEE32Array o = (IEEE32Array) other;
			for (int i = 0; i < count; i += 1) {
				storeIEEE32(i + myStart, iEEE32At(i + myStart) - o.iEEE32At(i + otherStart));
			}
		} else {
			super.subtractData(myStart, other, otherStart, count);
		}
	}

	protected void printElementOn(int index, PrintWriter oo) {
		oo.print(iEEE32At(index));
	}

	public int count() {
		return storage.length;
	}
}