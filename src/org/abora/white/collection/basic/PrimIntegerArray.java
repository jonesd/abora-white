/*
 * Abora hypertext system
 * Copyright 2003 David G Jones, david_jones@night.dircon.co.uk
 * 
 * Translated from Udanax-Gold source code: Copyright 1991 XOC, www.udanax.com
 */

package org.abora.white.collection.basic;

import org.abora.gold.java.missing.BooleanVar;
import org.abora.gold.java.missing.IntegerVar;
import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xpp.basic.Heaper;

/**
 * A common superclass for primitive arrays of integer types; this is the point
 * to add bulk operations for Boolean operations, etc if we ever want them
 */ 
public class PrimIntegerArray extends PrimDataArray {

	/**
	 * Constructor for PrimIntegerArray.
	 * @param count
	 * @param datumSize
	 */
	protected PrimIntegerArray(int count, int datumSize) {
		super(count, datumSize);
	}

	/**
	 * Store a new value into the array at the given index. If the value does
	 * not fit into the range that can be stored here, return an array of a kind
	 * that will accept it. If the index is past the end, return a larger array.
	 * If canModify, then returns a newly created array only if the value will
	 * not fit into this one, otherwise will always return a new array.
	 */ 
	public PrimIntegerArray hold(int index, IntegerVar value, BooleanVar canModify) {
		throw new UnsupportedOperationException();
	}

	public PrimIntegerArray hold(int index, IntegerVar value) {
		this(index, value, false);
	}

	/** Store an integer value */
	public void storeInteger(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual integer value */
	public IntegerVar integerAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeValue(int index, Heaper value) {
		throw new UnsupportedOperationException();
	}

	public Heaper fetchValue(int index) {
		throw new UnsupportedOperationException();
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
	}

	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	public int indexOf(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexOfInteger(IntegerVar value, int start, int count) {
		throw new UnsupportedOperationException();
	}

	public int indexOfInteger(IntegerVar value, int start) {
		return indexOfInteger(value, start, 1);
	}

	public int indexOfInteger(IntegerVar value) {
		return indexOfInteger(value, 0);
	}

	public int indexPast(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexPastInteger(IntegerVar value, int start, int count) {
		throw new UnsupportedOperationException();
	}

	public int indexPastInteger(IntegerVar value, int start) {
		return indexPastInteger(value, start, 1);
	}

	public int indexPastInteger(IntegerVar value) {
		return indexPastInteger(value, 0);
	}

	public void storeAll(Heaper value, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, Stream oo) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

}
