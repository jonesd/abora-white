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

import org.abora.white.x.PrimFloatValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * Array composed of floating point numbers of the same precision. 
 */
public abstract class PrimFloatArray extends PrimDataArray {

	protected PrimFloatArray() {
		super();
	}

	/** Make an array initialized to zero values */
	public static PrimFloatArray zeros(int bitCount, int count) {
		throw new UnsupportedOperationException();
	}

	/** Store a floating point value */
	public abstract void storeFloat(int index, double value);

	/** Get an actual floating point number */
	public abstract double floatAt(int index);

	public int elementsHash(int count, int start) {
		/* TODO make this actually do something !!!! */
		throw new UnsupportedOperationException();
		//			return this->getCategory()->hashForEqual() ^ ::fastHash(count);
		//		UInt32 PrimFloatArray::elementsHash (Int32 count/* = -1*/,
		//							 Int32 /*start*//* = Int32Zero*/)
		//		{
		//			/* make this actually do something !!!! */
		//			return this->getCategory()->hashForEqual() ^ ::fastHash(count);
		//		}
	}

	public int indexOf(Heaper value, int start, int nth) {
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
			for (int idx = start; idx < count(); idx++) {
				if (floatAt(idx) == x) {
					nth--;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx--) {
				if (floatAt(idx) == x) {
					nth++;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
		//		Int32 PrimFloatArray::indexOf (APTR(Heaper) value, 
		//						   Int32 start/* = Int32Zero*/,
		//						   Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			IEEE64 x = CAST(PrimFloatValue,value)->asIEEE64();
		//
		//			if (nth >= 0) {
		//			for (Int32 idx = start; idx < this->count(); idx++) {
		//				if (this->floatAt(idx) == x) {
		//				nth--;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			} else {
		//			for (Int32 idx = start; idx >= 0; idx--) {
		//				if (this->floatAt(idx) == x) {
		//				nth++;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
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
			for (int idx = start; idx < count(); idx++) {
				if (floatAt(idx) != x) {
					nth--;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx--) {
				if (floatAt(idx) != x) {
					nth++;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
		//		Int32 PrimFloatArray::indexPast (APTR(Heaper) value, 
		//						 Int32 start/* = Int32Zero*/,
		//						 Int32 nth/* = 1*/)
		//		{
		//			if (this->count() == 0 || nth == 0) {
		//			return -1;
		//			}
		//			if (start < 0) {
		//			start = this->count () + start;
		//			}
		//			if (start < 0 || start >= this->count ()) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//
		//			IEEE64 x = CAST(PrimFloatValue,value)->asIEEE64();
		//
		//			if (nth >= 0) {
		//			for (Int32 idx = start; idx < this->count(); idx++) {
		//				if (this->floatAt(idx) != x) {
		//				nth--;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			} else {
		//			for (Int32 idx = start; idx >= 0; idx--) {
		//				if (this->floatAt(idx) != x) {
		//				nth++;
		//				if (nth == 0) {
		//					return idx;
		//				}
		//				}
		//			}
		//			}
		//			return -1;
		//		}
	}

	protected int compareData(int start, PrimDataArray other, int otherStart, int count) {
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
		//		Int32 PrimFloatArray::compareData (Int32 start, 
		//						   APTR(PrimDataArray) other,
		//						   Int32 otherStart,
		//						   Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(PrimFloatArray,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				IEEE64 cmp;
		//				cmp = this->floatAt(i + start) - o->floatAt(i + otherStart);
		//				if (cmp != 0.0) {
		//					return ((Int32) cmp) < 0 ? -1 : 1;
		//				}
		//				}
		//				return 0;
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				return this->PrimDataArray::compareData (start, other, otherStart,
		//									 count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//			return 0;
		//		}
	}

	protected void addData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				storeFloat(i + start, floatAt(i + start) + o.floatAt(i + otherStart));
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
		//		void PrimFloatArray::addData (Int32 start, 
		//						  APTR(PrimDataArray) other,
		//						  Int32 otherStart,
		//						  Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(PrimFloatArray,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeFloat (i + start,
		//						  this->floatAt(i + start) 
		//						   + o->floatAt(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimDataArray::addData (start, other, otherStart, count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}

	protected void subtractData(int start, PrimDataArray other, int otherStart, int count) {
		if (other instanceof PrimFloatArray) {
			PrimFloatArray o = (PrimFloatArray) other;
			for (int i = 0; i < count; i += 1) {
				storeFloat(i + start, floatAt(i + start) - o.floatAt(i + otherStart));
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
		//		void PrimFloatArray::subtractData (Int32 start, 
		//						   APTR(PrimDataArray) other,
		//						   Int32 otherStart,
		//						   Int32 count)
		//		{
		//			BEGIN_CHOOSE(other) {
		//			BEGIN_KIND(PrimFloatArray,o) {
		//				for (Int32 i = 0; i < count; i += 1) {
		//				this->storeFloat (i + start,
		//						  this->floatAt(i + start) 
		//						  - o->floatAt(i + otherStart));
		//				}
		//			} END_KIND;
		//			BEGIN_OTHERS {
		//				this->PrimDataArray::subtractData (start, other, otherStart,
		//								   count);
		//			} END_OTHERS;
		//			} END_CHOOSE;
		//		}
	}
}
