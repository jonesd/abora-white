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
 * A common superclass for primitive arrays of arithmetic values
 * such as floats and integers.
 */
public abstract class PrimArithmeticArray extends PrimArray {
	//Implement the Comparable interface?

	/**
	 * Construct a new array.
	 * 
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 */
	protected PrimArithmeticArray() {
		super();
	}


	//////////////////////////////////////////////
	// arithmetic manipulations

	/**
	 * Arithmetic addition of the respective elements of other
	 * to this over the specified index range. 
	 * 
	 * @param to first index of receiver to be included
	 * @param other other elements to be added to the receivers elements
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all others elements starting at from and after. Fail if count is
	 * 			greater than number of elements in range.
	 * @param from first index of the other array to be included in the range
	 */
	public void addElements(int to, PrimArithmeticArray other, int count, int from) {
		int n = Math.min(other.count() - from, this.count() - to);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		addData(to, other, from, n);
	}

	/**
	 * Arithmetic addition of the respective elements of other
	 * to this over the specified index range. Others index range
	 * starts from its first element. 
	 * 
	 * @param to first index of receiver to be included
	 * @param other other elements to be added to the receivers elements
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all of others elements starting at from and after. Fail if count is
	 * 			greater than number of elements in range.
	 */
	public void addElements(int to, PrimArithmeticArray other, int count) {
		addElements(to, other, count, 0);
	}

	/**
	 * Arithmetic addition of the respective elements of other
	 * to this over the index range 
	 * 
	 * @param to first index of receiver to be included
	 * @param other other elements to be added to the receivers elements
	 */
	public void addElements(int to, PrimArithmeticArray other) {
		addElements(to, other, -1);
	}

	/**
	 * Arithmetic addition of the respective elements of other
	 * to this over the specified index range. 
	 * Subclasses should override.
	 */
	protected void addData(int start, PrimArithmeticArray other, int otherStart, int count) {
		//TODO called if receiver has incompatible other - try and convert to more general data type
		throw new UnsupportedOperationException();
	}

	/**
	 * Arithmetic subtraction of the respective elements of other
	 * from this over the specified index range. 
	 * 
	 * @param to first index of receiver to be included
	 * @param other other elements to be subtracted from the receivers elements
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all of others elements starting at from and after. Fail if count is
	 * 			greater than number of elements in range.
	 * @param from first index of the other array to be included
	 */
	public void subtractElements(int to, PrimArithmeticArray other, int count, int from) {
		int n = Math.min(other.count() - from, count() - to);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		subtractData(to, other, from, n);
	}

	/**
	 * Arithmetic subtraction of the respective elements of other
	 * from this over the specified index range. Others index range
	 * starts from its first element. 
	 * 
	 * @param to first index of receiver to be included
	 * @param other other elements to be subtracted from the receivers elements
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all of others elements starting at from and after. Fail if count is
	 * 			greater than number of elements in range.
	 */
	public void subtractElements(int to, PrimArithmeticArray other, int count) {
		subtractElements(to, other, count, 0);
	}


	/**
	 * Arithmetic subtraction of the respective elements of other
	 * from this over the specified index range. 
	 * 
	 * @param to first index of receiver to be included
	 * @param other other elements to be subtracted from the receivers elements
	 */
	public void subtractElements(int to, PrimArithmeticArray other) {
		subtractElements(to, other, -1);
	}

	/**
	 * Subtract the respective elements of other from this over the given index
	 * range.
	 * Subclasses should override.
	 */
	protected void subtractData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		//TODO called if receiver has incompatible other - try and convert to more general data type
		throw new UnsupportedOperationException();
	}


	//////////////////////////////////////////////
	// testing
	
	/**
	 * Return -1, 0, or +1 according to whether the elements in the specified
	 * span of this array are lexically less than, equal to, or greater than the
	 * specified span of the other. The other array must be of a compatible
	 * type. If the count is negative or goes beyond the end of either array,
	 * then the shorter array is considered to be extended with zeros.
	 * 
	 * NOTE: Because of zero extension, this is not the same as elementsEqual; it is
	 * possible that a->compare (b) == 0 even though ! a->contentsEqual (b)
	 * 
	 * @param other elements to compare the receivers elements against
	 * @param count number of elements in span to consider, or -1 for all elements
	 * 			after here and there. Zero elements are used to extend if there are
	 * 			insufficient elements in either array to fullfill the count.
	 * @param here inclusive start index of the receivers span
	 * @param there inclusive start index of the other arrays span
	 */
	public int compare(PrimArithmeticArray other, int count, int here, int there) {
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
		PrimArithmeticArray longer;
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
	}

	public int compare(PrimArithmeticArray other, int count, int here) {
		return compare(other, count, here, 0);
	}

	public int compare(PrimArithmeticArray other, int count) {
		return compare(other, count, 0);
	}

	public int compare(PrimArithmeticArray other) {
		return compare(other, -1);
	}

	/**
	 * Return the sign, -1 or +1, of the next non-zero element after start, or 0 if no such
	 * element.  Note that for the unsigned arrays, this will only return 0 or
	 * 1.
	 */
	protected abstract int signOfNonZeroAfter(int start);

	public boolean contentsEqual(PrimArray other) {
		if (count() != other.count()) {
			return false;
		}
		return compareData(0, (PrimArithmeticArray) other, 0, count()) == 0;
		//		BooleanVar PrimDataArray::contentsEqual (PrimArray * other){
		//			if (this->count() != other->count()) {
		//			return FALSE;
		//			}
		//			return this->compareData (0, CAST(PrimDataArray,other),
		//						  0, this->count())
		//					== 0;
		//		}
	}

	public boolean elementsEqual(int here, PrimArray other, int there, int count) {
		int n = Math.min(other.count() - there, count() - here);
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		return compareData(here, (PrimArithmeticArray) other, there, n) == 0;
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
	 * Over given range, returns - if this < other; 0 if this == other; + if
	 * this > other.
	 */
	protected int compareData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}
}
