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

/**
 * A common superclass for primitive arrays of basic data
 * types (i.e. bits of some kind)
 */
public abstract class PrimDataArray extends PrimArray {

	protected PrimDataArray() {
		super();
	}

	//	public PrimSpec spec () {
	//		throw new UnsupportedOperationException();
	//	}

	public boolean contentsEqual(PrimArray other) {
		if (count() != other.count()) {
			return false;
		}
		return compareData(0, (PrimDataArray) other, 0, count()) == 0;
		//		BooleanVar PrimDataArray::contentsEqual (PrimArray * other){
		//			if (this->count() != other->count()) {
		//			return FALSE;
		//			}
		//			return this->compareData (0, CAST(PrimDataArray,other),
		//						  0, this->count())
		//					== 0;
		//		}
	}

	/**
	 * Overwrite receivers elements within the specified range by the result
	 * of the arithmetic addition of each receiver element and the
	 * corresponding other arrays element.
	 * 
	 * @param to first index of receiver to be overwritten
	 * @param other other elements to be added to the receivers elements
	 * @param count number of elements from the other array to overwire, or -1 for
	 * 			all of others elements starting at from and after
	 * @param from first index of the other array to be included from
	 */
	public void addElements(int to, PrimDataArray other, int count, int from) {
		int n = Math.min(other.count() - from, this.count() - to);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		addData(to, other, from, n);
		//		void PrimDataArray::addElements (Int32 to,
		//						 APTR(PrimDataArray) other,
		//						 Int32 count/* = -1*/,
		//						 Int32 from/* = Int32Zero*/)
		//		{
		//			Int32 n;
		//
		//			n = min(other->count() - from, this->count() - to);
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			this->addData (to, other, from, n);
		//		}
	}

	public void addElements(int to, PrimDataArray other, int count) {
		addElements(to, other, count, 0);
	}

	public void addElements(int to, PrimDataArray other) {
		addElements(to, other, -1);
	}

	public void subtractElements(int to, PrimDataArray other, int count, int from) {
		int n = Math.min(other.count() - from, count() - to);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		subtractData(to, other, from, n);
		//		void PrimDataArray::subtractElements (Int32 to,
		//							APTR(PrimDataArray) other,
		//							Int32 count/* = -1*/,
		//							Int32 from/* = Int32Zero*/)
		//		{
		//			Int32 n;
		//
		//			n = min(other->count() - from, this->count() - to);
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			this->subtractData (to, other, from, n);
		//		}
	}

	public void subtractElements(int to, PrimDataArray other, int count) {
		subtractElements(to, other, count, 0);
	}

	public void subtractElements(int to, PrimDataArray other) {
		subtractElements(to, other, -1);
	}

	/**
	 * Return -1, 0, or +1 according to whether the elements in the specified
	 * span of this array are lexically less than, equal to, or greater than the
	 * specified span of the other. The other array must be of a compatible
	 * type. If the count is negative or goes beyond the end of either array,
	 * then the shorter array is considered to be extended with zeros. 	NOTE:
	 * Because of zero extension, this is not the same as elementsEqual; it is
	 * possible that a->compare (b) == 0 even though ! a->contentsEqual (b)
	 */
	public int compare(PrimDataArray other, int count, int here, int there) {
		int n = Math.min(other.count() - there, count() - here);
		if (count >= 0 && count < n) {
			n = count;
		}
		int cmp = compareData(here, other, there, n);
		if (cmp != 0) {
			return cmp < 0 ? -1 : 1;
		}

		/* past the end. check if a count was specified or if they 
		are the same length */
		if (count == n || (other.count() - there) == (count() - here)) {
			return 0;
		}
		/* figure out which is longer and look for a nonzero element */
		PrimDataArray longer;
		int index;
		if (n == other.count() - there) {
			longer = this;
			index = here + n;
		} else {
			longer = other;
			index = there + n;
		}
		if (count >= 0) {
			n = Math.min(count - n + index, longer.count());
		} else {
			n = longer.count();
		}
		int sign = longer.signOfNonZeroAfter(index);
		return longer == this ? sign : -sign;
		//		Int32 PrimDataArray::compare (APTR(PrimDataArray) other,
		//						  Int32 count/* = -1*/,
		//						  UInt32 here/* = Int32Zero*/,
		//						  Int32 there/* = Int32Zero*/)
		//		{
		//			/* Return -1, 0, or +1 according to whether the elements in 
		//			the specified span of this array are lexically less than, 
		//			equal to, or greater than the specified span of the other. 
		//			The other array must be of a compatible type. If the count is 
		//			negative or goes beyond the end of either array, then the 
		//			shorter array is considered to be extended with zeros.
		//			NOTE: Because of zero extension, this is not the same as 
		//			elementsEqual; it is possible that a->compare (b) == 0 even 
		//			though ! a->contentsEqual (b) */
		//
		//			Int32 n;
		//			SPTR(PrimDataArray) longer;
		//			Int32 index;
		//			Int32 cmp;
		//
		//			n = min(other->count() - there, this->count() - here);
		//			if (count >= 0 && count < n) {
		//			n = count;
		//			}
		//			cmp = this->compareData (here, other, there, n);
		//			if (cmp != 0) {
		//			return cmp < 0 ? -1 : 1;
		//			}
		//
		//			/* past the end. check if a count was specified or if they 
		//			are the same length */
		//			if (count == n || (other->count() - there) == (this->count() -(Int32) here)) {
		//			return 0;
		//			}
		//			/* figure out which is longer and look for a nonzero element */
		//			if (n == other->count() - there) {
		//			longer = this;
		//			index = here + n;
		//			} else {
		//			longer = other;
		//			index = there + n;
		//			}
		//			if (count >= 0) {
		//			n = min(count - n + index, longer->count());
		//			} else {
		//			n = longer->count();
		//			}
		//			Int32 sign = longer->signOfNonZeroAfter (index);
		//			return longer == this ? sign : -sign;
		//		}
	}

	/**
	 * return the sign of the next non-zero element after start, or 0 if no such
	 * element.  Note that for the unsigned arrays, this will only return 0 or
	 * 1.
	 */
	protected abstract int signOfNonZeroAfter(int start);

	public int compare(PrimDataArray other, int count, int here) {
		return compare(other, count, here, 0);
	}

	public int compare(PrimDataArray other, int count) {
		return compare(other, count, 0);
	}

	public int compare(PrimDataArray other) {
		return compare(other, -1);
	}

	public boolean elementsEqual(int here, PrimArray other, int there, int count) {
		int n = Math.min(other.count() - there, count() - here);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		return compareData(here, (PrimDataArray) other, there, n) == 0;
		//		BooleanVar PrimDataArray::elementsEqual (Int32 here,
		//							 APTR(PrimArray) other,
		//							 Int32 there/* = Int32Zero*/,
		//							 Int32 count/* = -1*/)
		//		{
		//			Int32 n;
		//
		//			n = min(other->count() - there, this->count() - here);
		//			if (count > n) {
		//			BLAST(IndexOutOfBounds);
		//			}
		//			if (count >= 0) {
		//			n = count;
		//			}
		//			return this->compareData (here, CAST(PrimDataArray,other),
		//						  there, n) == 0;
		//		}
	}

	/**
	 * over given range, returns - if this < other; 0 if this == other; + if
	 * this > other.
	 */
	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Add the respective elements of other to this over the given index range.
	 */
	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Subtract the respective elements of other from this over the given index
	 * range.
	 */
	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}
}
