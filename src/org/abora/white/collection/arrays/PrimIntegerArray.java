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

import org.abora.white.x.PrimIntValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * A common superclass for primitive arrays of integer types; this is the point
 * to add bulk operations for Boolean operations, etc if we ever want them
 */
public abstract class PrimIntegerArray extends PrimDataArray {

	protected PrimIntegerArray() {
		super();
	}

	/**
	 * Store a new value into the array at the given index. If the value does
	 * not fit into the range that can be stored here, return an array of a kind
	 * that will accept it. If the index is past the end, return a larger array.
	 * If canModify, then returns a newly created array only if the value will
	 * not fit into this one, otherwise will always return a new array.
	 */
	public PrimIntegerArray hold(int index, PrimIntValue value, boolean canModify) {
		throw new UnsupportedOperationException();
	}

	public PrimIntegerArray hold(int index, PrimIntValue value) {
		return hold(index, value, false);
	}

	/** Store an integer value */
	public abstract void storeInteger(int index, PrimIntValue value);

	/** Get an actual integer value */
	public abstract PrimIntValue integerAt(int index);

	public abstract void storeValue(int index, Heaper value);

	public abstract Heaper fetchValue(int index);

	//	public PrimSpec spec() {
	//		throw new UnsupportedOperationException();
	//	}

	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	public int indexOf(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexOfInteger(int value, int start, int count) {
		throw new UnsupportedOperationException();
	}

	public int indexOfInteger(int value, int start) {
		return indexOfInteger(value, start, 1);
	}

	public int indexOfInteger(int value) {
		return indexOfInteger(value, 0);
	}

	public int indexPast(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexPastInteger(PrimIntValue value, int start, int count) {
		throw new UnsupportedOperationException();
	}

	public int indexPastInteger(PrimIntValue value, int start) {
		return indexPastInteger(value, start, 1);
	}

	public int indexPastInteger(PrimIntValue value) {
		return indexPastInteger(value, 0);
	}

	public void storeAll(Heaper value, int count, int start) {
		throw new UnsupportedOperationException();
	}

	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}
}
