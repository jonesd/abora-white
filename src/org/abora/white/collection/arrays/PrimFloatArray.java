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

import org.abora.white.value.PrimFloatValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * Fixed size array containing floating point numbers of the same precision.
 * <p>
 * The <code>double</code> primitive type is assumed to have the greatest
 * precision stored by all subclass implementations and so is used to implement
 * general floating-point implementations. Subclasses should reimplement operations for
 * performance or where their specific element type has to be reflected in the
 * API.
 */
public abstract class PrimFloatArray extends PrimArithmeticArray {

	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new array.
	 * <p>
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 */
	protected PrimFloatArray() {
		super();
	}

//	/** Make an array initialized to zero values */
//	public static PrimFloatArray zeros(int bitCount, int count) {
//		throw new UnsupportedOperationException();
//	}
//

	//////////////////////////////////////////////
	// Accessing
	
	/** 
	 * Store a floating point number at the specified <code>index</code>.
	 * 
	 * @param index 0 based index to store the <code>value</code> at.
	 * @param value value to store in array, may lose precision if
	 * 	array cant hold the full extent of it. 
	 */
	public abstract void storeFloat(int index, double value);

	/**
	 * Return the floating point number at the specified <code>index</code>.
	 * 
	 * @param index 0 based index of this to read the number from.
	 * @return value at the specified index. 
	 */
	public abstract double floatAt(int index);


	//////////////////////////////////////////////
	// Comparing and Hashing
	
	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
//		//TODO double check implementation
//		int hash = 17;
//		for (int i = start; i < count; i += 1) {
//			double value = floatAt(i);
//			hash = hash * 37 + (int)value;
//		}
//		return hash;
		//			return this->getCategory()->hashForEqual() ^ ::fastHash(count);
		//		UInt32 PrimFloatArray::elementsHash (Int32 count/* = -1*/,
		//							 Int32 /*start*//* = Int32Zero*/)
		//		{
		//			/* make this actually do something !!!! */
		//			return this->getCategory()->hashForEqual() ^ ::fastHash(count);
		//		}
	}

	protected int compareData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				double cmp = floatAt(i + start) - o.floatAt(i + otherStart);
				if (cmp != 0.0) {
					return ((int) cmp) < 0 ? -1 : 1;
				}
			}
			return 0;
		} else {
			return super.compareData(start, other, otherStart, count);
		}
	}


	//////////////////////////////////////////////
	// Searching/Finding

	public int indexOf(Heaper value, int start, int nth) {
		//TODO contents of indexOf && indexPast are the same except
		// whether the value should, or shoud not, match elements
		// Refactor out private shared method with exclusive-or check
		
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		double x = ((PrimFloatValue) value).asIEEE64();

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				if (floatAt(idx) == x) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				if (floatAt(idx) == x) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
	}

	public int indexPast(Heaper value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		double x = ((PrimFloatValue) value).asIEEE64();

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				if (floatAt(idx) != x) {
					nth -= 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				if (floatAt(idx) != x) {
					nth += 1;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
	}


	//////////////////////////////////////////////
	// Arithmetic Manipulations

	protected void addData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				storeFloat(i + start, floatAt(i + start) + o.floatAt(i + otherStart));
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				storeFloat(i + start, floatAt(i + start) - o.floatAt(i + otherStart));
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}
}
