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
package info.dgjones.abora.white.collection.arrays;

import info.dgjones.abora.white.hash.FHash;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.value.PrimIntegerSpec;
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A common superclass for primitive arrays of integer types; this is the point
 * to add bulk operations for Boolean operations, etc if we ever want them
 */
public abstract class PrimIntegerArray extends PrimArithmeticArray {

	//////////////////////////////////////////////
	// Constructors

	protected PrimIntegerArray() {
		super();
	}

	//////////////////////////////////////////////
	// accessing

	/**
	 * Store an integer <code>value</code> at the specified <code>index</code>.
	 * 
	 * @param index index in array the element will be stored at.
	 * @param value integer to store in <code>this</code>.
	 * @throws IllegalArgumentException if value can not be held by array
	 */
	public abstract void storeInteger(int index, IntegerValue value);

	/**
	 * Fetch an integer value at the specified <code>index</code>.
	 * 
	 * @param index index in array whose element will be returned
	 * @return the integer at the specified <code>index</code>.
	 */
	public abstract IntegerValue integerAt(int index);

	/**
	 * Store an integer <code>value</code> at the specified <code>index</code>.
	 * If <code>index</code> is past the end of the current array, then
	 * return a copy of the array that has been extended to include the specified <code>index</code>.
	 * Fill new intervening elements with zero.
	 * If <code>value</code> can not be held by the current array, then return
	 * a copy of the array of a kind that can hold the value.
	 * If <code>canModify</code> and the value can be stored in the current array
	 * then modify this array, otherwise in all other cases return a copy of the
	 * this array.
	 * 
	 * @param index index in array the element will be stored at.
	 * @param value integer to store in array
	 * @param canModify true if the the value can be stored in <code>this</code> array,
	 * 	if possible, otherwise always return a copy of <code>this</code> array.
	 * @return array holding <code>value</code>, may be this array or a copy.
	 */
	public PrimIntegerArray hold(int index, IntegerValue value, boolean canModify) {
		if (index < 0) {
			throw new IndexOutOfBoundsException();
		}
		PrimArray result;
		if (index >= count()) {
			if (((PrimIntegerSpec) spec()).canHold(value)) {
				result = spec().copyGrow(this, index + 1 - count());
			} else {
				result = PrimSpec.toHold(value).copyGrow(this, index + 1 - count());
			}
		} else {
			if (((PrimIntegerSpec) spec()).canHold(value)) {
				if (canModify) {
					result = this;
				} else {
					result = copy();
				}
			} else {
				result = PrimSpec.toHold(value).copy(this);
			}
		}
		((PrimIntegerArray) result).storeInteger(index, value);
		return (PrimIntegerArray) result;
	}

	public PrimIntegerArray hold(int index, IntegerValue value) {
		return hold(index, value, false);
	}

	//////////////////////////////////////////////
	// Searching/Finding

	public int indexOf(Heaper value, int start, int n) {
		return indexOfInteger((IntegerValue) value, start, n);
		//		}
	}

	public int indexOfInteger(IntegerValue value, int start, int nth) {
		if (count() == 0 || nth == 0) {
			return -1;
		}
		if (start < 0) {
			start = count() + start;
		}
		if (start < 0 || start >= count()) {
			throw new IndexOutOfBoundsException();
		}

		if (nth >= 0) {
			for (int idx = start; idx < count(); idx += 1) {
				if (integerAt(idx).isEqual(value)) {
					nth--;
					if (nth == 0) {
						return idx;
					}
				}
			}
		} else {
			for (int idx = start; idx >= 0; idx -= 1) {
				if (integerAt(idx).isEqual(value)) {
					nth++;
					if (nth == 0) {
						return idx;
					}
				}
			}
		}
		return -1;
	}

	public int indexOfInteger(IntegerValue value, int start) {
		return indexOfInteger(value, start, 1);
	}

	public int indexOfInteger(IntegerValue value) {
		return indexOfInteger(value, 0);
	}

	public int indexPast(Heaper value, int start, int n) {
		return indexPastInteger((IntegerValue) value, start, n);
	}

	public int indexPastInteger(IntegerValue value, int start, int nth) {
		//TODO compare contents of this method with PrimFloatArray

		if (count() == 0 || nth == 0) {
			return -1;
		}
		int result;
		if (start < 0) {
			result = count() + start;
		} else {
			result = start;
		}
		if (result < 0 || result >= count()) {
			throw new IndexOutOfBoundsException();
		}

		int n;
		if (nth >= 0) {
			n = nth;
			do {
				if (!value.isEqual(integerAt(result))) {
					n = n - 1;
					if (n == 0) {
						return result;
					}
				}
				result = result + 1;
			} while (result < count());
			return -1;
		} else {
			n = nth;
			do {
				if (!value.isEqual(integerAt(result))) {
					n = n + 1;
					if (n == 0) {
						return result;
					}
				}
				result = result - 1;
			} while (result >= 0);
			return -1;
		}
	}

	public int indexPastInteger(IntegerValue value, int start) {
		return indexPastInteger(value, start, 1);
	}

	public int indexPastInteger(IntegerValue value) {
		return indexPastInteger(value, 0);
	}

	//////////////////////////////////////////////
	// Bulk Storing

	public void storeAll(Heaper value, int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		IntegerValue k;
		if (value == null) {
			k = IntegerValue.zero();
		} else {
			k = (IntegerValue) value;
		}
		for (int i = 0; i < n; i += 1) {
			storeInteger(start + i, k);
		}
	}

	//////////////////////////////////////////////
	// Comparing and Hashing

	protected int compareData(int here, PrimArithmeticArray other, int there, int count) {
		if (other instanceof PrimIntegerArray) {
			PrimIntegerArray o = (PrimIntegerArray) other;
			for (int i = 0; i < count; i += 1) {
				IntegerValue a = integerAt(here + i);
				IntegerValue b = o.integerAt(there + i);
				int comparison = a.compareTo(b);
				if (comparison != 0) {
					return comparison;
				}
			}
			return 0;
		} else {
			return super.compareData(here, other, there, count);
		}
	}

	public int elementsHash(int count, int start) {
		int n = count() - start;
		if (count > n) {
			throw new IndexOutOfBoundsException();
		}
		if (count >= 0) {
			n = count;
		}
		if (n == 0) {
			return FHash.hashInt(17);
		} else {
			if (n == 1) {
				return FHash.hashInt(integerAt(start).asInt32());
			} else {
				return FHash.hashInt(n) ^ FHash.hashInt(integerAt(start).asInt32()) ^ FHash.hashInt(integerAt(start + n - 1).asInt32());
			}
		}
	}

	//////////////////////////////////////////////
	// Helper methods

	protected Heaper zeroElement() {
		return IntegerValue.zero();
	}

	//////////////////////////////////////////////
	// Arithmetic Operations

	protected void addData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof PrimIntegerArray) {
			PrimIntegerArray o = (PrimIntegerArray) other;
			for (int i = 0; i < count; i += 1) {
				IntegerValue sum = integerAt(i + start).plus(o.integerAt(i + otherStart));
				storeInteger(i + start, sum);
			}
		} else {
			super.addData(start, other, otherStart, count);
		}
	}

	protected void subtractData(int start, PrimArithmeticArray other, int otherStart, int count) {
		if (other instanceof PrimIntegerArray) {
			PrimIntegerArray o = (PrimIntegerArray) other;
			for (int i = 0; i < count; i += 1) {
				IntegerValue sum = integerAt(i + start).minus(o.integerAt(i + otherStart));
				storeInteger(i + start, sum);
			}
		} else {
			super.subtractData(start, other, otherStart, count);
		}
	}
}
