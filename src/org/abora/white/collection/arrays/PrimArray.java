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

import org.abora.white.exception.CopyOutOfBoundsException;
import org.abora.white.exception.NotInTableException;
import org.abora.white.xpp.basic.Heaper;

/**
 * Array objects in smalltalk vary in size, while in x++ the
 * allocate separate storage for the varying part.  Thus they
 * require very different Recipes.
 *
 * Recipes are normally registered by the class'' associated
 * Recipe class at its own initialization time.  In smalltalk,
 * the Array classes share a common Recipe class,
 * (STArrayRecipe), which registers an instance of itself for
 * each appropriate concrete subclass of PrimArray.X.
 * 
 * (WeakPtrArrays are not in the browser because they currently 
 * have no clients and may disappear.)
 */
public abstract class PrimArray extends Heaper {
	//TODO Implement Collection?

	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new array.
	 * 
	 * Restrict public access to constructor; use suitable static
	 * factory method instead.  
	 */
	protected PrimArray() {
		super();
	}

	protected abstract PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset);


	/**
	 * Return the number of elements the array can hold.
	 */
	public abstract int count();

//	/** 
//	 * A description of the kinds of things which can be stored 
//	 * in  this array
//	 */
//	public abstract PrimSpec spec();


	//////////////////////////////////////////////
	// Accessing
	
	/**
	 * Store a value; may be a Heaper, null, or a PrimValue as appropriate
	 * to PrimArray subclass.
	 * 
	 * It is expected that most PrimArray clients will want to use
	 * less abstract access methods
	 */
	public abstract void storeValue(int index, Heaper value);

	/** 
	 * Fetch a value; may be a Heaper, null, or a PrimValue as appropriate
	 * to PrimArray subclass.
	 * 
	 * It is expected that most PrimArray clients
	 * will want to use less abstract access methods.
	 * 
	 * @param index index in array whose element will be returned
	 */
	public abstract Heaper fetchValue(int index);

	/** 
	 * Fetch a value; may be a Heaper or a PrimValue as appropriate
	 * to PrimArray subclass, or throw an exception if value is null.
	 * 
	 * It is expected that most PrimArray clients will want to use
	 * less abstract access methods.
	 * 
	 * @param index index in array whose element will be returned
	 * @throws NotInTableException if a null value was fetched
	 */
	public Heaper getValue(int index) {
		Heaper result = fetchValue(index);
		if (result == null) {
			throw new NotInTableException();
		}
		return result;
	}
	

	//////////////////////////////////////////////
	// comparison and hash

	//TODO made up
	public boolean isEqual(Heaper other) {
		if (other instanceof PrimArray) {
			PrimArray o = (PrimArray) other;
			return contentsEqual(o);
		} else {
			return false;
		}
	}

	/** 
	 * Whether the two ranges contain semantically the same 
	 * values.   Two non-NULL pointers match iff the Heapers they
	 * point  to are isEqual. Two integers match iff they have the  same  value,
	 * even though they may be represented as different sizes. Two floats
	 * likewise.
	 */
	public abstract boolean contentsEqual(PrimArray other);

	/**
	 * A hash of the entire contents of the array. If two arrays are
	 * contentsEqual, they will have the same contentsHash.
	 */
	public int contentsHash() {
		return elementsHash();
	}

	/**
	 * Whether the two ranges contain the same values, using the criteria
	 * defined in contentsEqual
	 */
	public abstract boolean elementsEqual(int here, PrimArray other, int there, int n);

	public boolean elementsEqual(int here, PrimArray other, int there) {
		return elementsEqual(here, other, there, -1);
	}

	public boolean elementsEqual(int here, PrimArray other) {
		return elementsEqual(here, other, 0);
	}

	/**
	 * A hash of the range of values out of the array. If two ranges are
	 * elementsEqual, they will have the same hash. For data values, additional
	 * zeros on the end make no difference to the hash.
	 */
	public abstract int elementsHash(int count, int start);

	public int elementsHash(int count) {
		return elementsHash(count, 0);
	}

	public int elementsHash() {
		return elementsHash(-1);
	}


	//////////////////////////////////////////////
	// Bulk storing

	/** 
	 * Set a range of elements to have the same value
	 */
	//TODO possibly rename to fill(...) to better match Java terminology
	public abstract void storeAll(Heaper value, int count, int start);

	public void storeAll(Heaper value, int count) {
		storeAll(value, count, 0);
	}

	public void storeAll(Heaper value) {
		storeAll(value, -1);
	}

	public void storeAll() {
		storeAll(null);
	}

	
	/**
	 * Copy the respective elements of other to this over the specified index range.
	 * The other array must be of a compatible type. 
	 * 
	 * @param to first index of receiver to be included in the receivers range..
	 * @param other other elements to be copied into this.
	 * @param count number of elements from the other array included in range, or -1 for
	 * 			all others elements starting at from. Fail if count is
	 * 			greater than number of available elements.
	 * @param from first index of the other array to be included in the range.
	 */ 
	public void storeMany(int to, PrimArray other, int count, int from) {
		int n;

		if (count < 0) {
			n = other.count() - from;
		} else {
			n = count;
		}
		if (to + n > count() || from + n > other.count()) {
			throw new IndexOutOfBoundsException();
		}
		copyElements(to, other, from, n);
	}

	/**
	 * Copy n elements from the other array into this one. The other  array must
	 * be of a compatible type.
	 */
	public void storeMany(int to, PrimArray other, int count) {
		storeMany(to, other, count, 0);
	}

	/**
	 * Copy n elements from the other array into this one. The other  array must
	 * be of a compatible type.
	 */
	public void storeMany(int to, PrimArray other) {
		storeMany(to, other, -1);
	}

	/**
	 * Return a copy of this including just the elements specified by
	 * the index range prepended by before, and postpended by after
	 * number of null or 0 elements.
	 * <p>
	 * If before = 10, then the the resulting array with be 10 elements
	 * larger and the copied elements would start at index 10.
	 * 
	 * @param count number of elements to copy, or -1 for all from and after start
	 * @param start index of first element to copy from
	 * @param before number of leading null or 0 elements to include before the copied
	 * 	elements in the returned array.
	 * @param after number of trailing null or 0 elements to include after the copied
	 * 	elements in the returned array.
	 * @return PrimArray a copy of this.
	 */
	public PrimArray copy(int count, int start, int before, int after) {
		int copyCount;

		if (count < 0) {
			copyCount = count() - start;
		} else {
			copyCount = count;
			if (start + copyCount > count()) {
				throw new IndexOutOfBoundsException();
			}
		}
		return makeNew(copyCount + before + after, this, start, copyCount, before);
	}

	/**
	 * Return a copy of this including just the elements specified by
	 * the index range prepended by before number of null or 0 elements.
	 * <p>
	 * If before = 10, then the the resulting array with be 10 elements
	 * larger and the copied elements would start at index 10.
	 * 
	 * @param count number of elements to copy, or -1 for all from and after start
	 * @param start index of first element to copy from
	 * @param before number of leading null or 0 elements in include before the copied
	 * 	elements in the returned array.
	 * @return PrimArray a copy of this.
	 */
	public PrimArray copy(int count, int start, int before) {
		return copy(count, start, before, 0);
	}

	/**
	 * Return a copy of this including just the elements specified by
	 * the index range.
	 * 
	 * @param count number of elements to copy, or -1 for all from and after start
	 * @param start index of first element to copy from
	 * @return PrimArray a copy of this
	 */
	public PrimArray copy(int count, int start) {
		return copy(count, start, 0);
	}

	/**
	 * Return a copy of this including just the first count elements.
	 * 
	 * @param count
	 * @return PrimArray a copy of this.
	 */
	public PrimArray copy(int count) {
		return copy(count, 0);
	}

	/**
	 * Return a copy of this.
	 * 
	 * @return a copy of this.
	 */
	public PrimArray copy() {
		return copy(-1);
	}

	/**
	 *  Make a copy of the array into a larger array.  The array has
	 * 'after' slots after the copied elements.
	 * 
	 * @param after number of trailing null or 0 elements to include after the copied
	 * 	elements in the returned array.
	 */
	public PrimArray copyGrow(int after) {
		return copy(count(), 0, 0, after);
	}


	//////////////////////////////////////////////
	// Searching/Finding

	/**
	 * Return the index of the nth occurrence of the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @param nth nth occurrence of the matched value at or after the start if
	 * 			positive, or at or before if negative. A 0 nth immediately fails.
	 * @return index of element matched or -1 if there is none
	 */
	public abstract int indexOf(Heaper value, int start, int n);

	/**
	 * Return the index of the first occurrence of the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @return index of element matched or -1 if there is none
	 */
	public int indexOf(Heaper value, int start) {
		return indexOf(value, start, 1);
	}

	/**
	 * Return the index of the first occurrence of the given value,
	 *  or -1 if there is none.
	 * 
	 * @param value element that is to be matched
	 * @return index of element matched or -1 if there is none
	 */
	public int indexOf(Heaper value) {
		return indexOf(value, 0);
	}

	/**
	 * Return the index of the nth occurrence of anything but the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value anything except this element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @param nth nth occurrence of the matched value at or after the start if
	 * 			positive, or at or before if negative. A 0 nth immediately fails.
	 * @return index of element matched or -1 if there is none
	 */
	public abstract int indexPast(Heaper value, int start, int n);
	//TODO original X++ version had an implementation but it did
	// not appear to match spec, or inherited versions, so removed

	/**
	 * Return the index of the first occurrence of anything but the given value at or
	 * after (before if nth is negative) the given index, or -1 if
	 * there is none.
	 * 
	 * @param value anything except this element that is to be matched
	 * @param start index to start the search. If positive start from that index,
	 * 			if negative start from relatie to end of array
	 * @return index of element matched or -1 if there is none
	 */
	public int indexPast(Heaper value, int start) {
		return indexPast(value, start, 1);
	}

	/**
	 * Return the index of the first occurrence of anything but the given value,
	 * or -1 if there is none.
	 * 
	 * @param value anything except this element that is to be matched
	 * @return index of element matched or -1 if there is none
	 */
	public int indexPast(Heaper value) {
		return indexPast(value, 0);
	}


	/**
	 * The index of the nth occurrence of the given sequence of values at or
	 * after (before if n is negative) the given starting index, or -1 if there
	 * is none. Negative numbers for start are relative to the end of the array.
	 */
	public int indexOfElements(PrimArray other, int valueCount, int valueStart, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		int valueN;
		if (valueCount < 0) {
			valueN = other.count();
		} else {
			if (valueCount > other.count()) {
				throw new IndexOutOfBoundsException();
			}
			valueN = valueCount;
		}
		int result;
		if (start >= 0) {
			result = start;
		} else {
			result = count() + start;
		}
		int n = nth < 0 ? -nth : nth; //TODO = Math.abs(nth);
		boolean forward = nth > 0;
		for (;;) {
			{
				if (forward ? (result > count() - valueN) : result < 0) {
					return -1;
				}
				if (elementsEqual(result, other, valueStart, valueN)) {
					n -= 1;
					if (n == 0) {
						return result;
					}
				}
				if (forward) {
					result += 1;
				} else {
					result -= 1;
				}
			}
		}
//		int valueN;
//		int result;
//		int n;
//
//		if (count() == 0 || nth == 0) {
//			return -1;
//		}
//		if (valueCount < 0) {
//			valueN = other.count();
//		} else {
//			if (valueCount > other.count()) {
//				throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
//			}
//			valueN = valueCount;
//		}
//		if (start >= 0) {
//			result = start;
//		} else {
//			result = count() + start;
//		}
//		n = nth < 0 ? -nth : nth;
//		boolean forward = nth > 0;
//		for (;;) {
//			{
//				if (forward ? (result > count()) - valueN : result < 0) {
//					return -1;
//				}
//				if (elementsEqual(result, other, valueStart, valueN).asBoolean()) {
//					n -= 1;
//					if (n == 0) {
//						return result;
//					}
//				}
//				if (forward) {
//					result += 1;
//				} else {
//					result -= 1;
//				}
//			}
//		}
	}

	public int indexOfElements(PrimArray other, int valueCount, int valueStart, int start) {
		return indexOfElements(other, valueCount, valueStart, start, 1);
	}

	public int indexOfElements(PrimArray other, int valueCount, int valueStart) {
		return indexOfElements(other, valueCount, valueStart, 0);
	}

	public int indexOfElements(PrimArray other, int valueCount) {
		return indexOfElements(other, valueCount, 0);
	}

	public int indexOfElements(PrimArray other) {
		return indexOfElements(other, -1);
	}


	//////////////////////////////////////////////
	// Debug

	public void printOn(PrintWriter oo) {
		String before = "[";
		if (count() == 0) {
			oo.print("[empty");
		} else {
			for (int i = 0; i < count(); i += 1) {
				oo.print(before);
				printElementOn(i, oo);
				before = " ";
			}
		}
		oo.print("]");
	}

	protected void printElementOn(int index, PrintWriter oo) {
		throw new UnsupportedOperationException();
	}

	//TODO remove this if we dont end up using it
	protected int rangeCheck(int index) {
		if (index < 0 || index >= count()) {
			throw new IndexOutOfBoundsException();
		}
		return index;
	}

	/**
	 * subclasses with non-32 bit or other interesting values should override
	 */
	protected void copyElements(int to, PrimArray source, int from, int count) {
		int n = count;
		if (n == -1) {
			n = source.count() - from;
		}
		if (n < 0 || to < 0 || from < 0 || from + n > source.count() || to + n > count()) {
			throw new CopyOutOfBoundsException();
		}
//		if (getCategory() == source.getCategory()) {
//			/* we can hold the source storage pointer since this is atomic */
//			for (int i = 0; i < n; i += 1) {
//				myStorage[to + i] = source.storage()[from + i];
//			}
//		} else {
			/* since the types aren''t the same, we have to do it the hard way */
			for (int i = 0; i < n; i += 1) {
				storeValue(to + i, source.fetchValue(from + i));
//			}
		}
	}

	protected void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
//		int n = count;
//
//		if (n < 0) {
//			n = count();
//		}
//		if (n + from > count()) {
//			throw new AboraRuntimeException(AboraRuntimeException.TOO_MANY_ZEROS);
//		}
//		if (from < 0) {
//			throw new AboraRuntimeException(AboraRuntimeException.BOGUS_START_INDEX);
//		}
//		Arrays.fill(myStorage, from, from + n, 0);
	}

	protected void zeroElements(int from) {
		zeroElements(from, -1);
	}

	protected void zeroElements() {
		zeroElements(0);
	}
}