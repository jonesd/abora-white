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

import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class IntegerVarArray extends PrimIntegerArray {
	private IntegerValue[] storage;

	//	IntegerVarArray (Int32 count, TCSJ);

	protected IntegerVarArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected IntegerVarArray(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create an IntegerVarArray filled with zeros */
	public static IntegerVarArray zeros(int count) {
		throw new UnsupportedOperationException();
	}

	/** create an IntegerVarArray filled with the indicated data in 'from' */
	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static IntegerVarArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static IntegerVarArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create an IntegerVarArray filled with the data at 'buffer' */
	public static IntegerVarArray make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** Store an actual integer value */
	public void storeIntegerVar(int index, IntegerValue value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual integer value */
	public IntegerValue integerVarAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeInteger(int index, IntegerValue value) {
		throw new UnsupportedOperationException();
	}

	public IntegerValue integerAt(int index) {
		throw new UnsupportedOperationException();
	}

	public void storeValue(int index, Heaper value) {
		throw new UnsupportedOperationException();
	}

	public Heaper fetchValue(int index) {
		throw new UnsupportedOperationException();
	}

	//	public PrimSpec spec() {
	//		throw new UnsupportedOperationException();
	//	}

	public void copyToBuffer(int[] buffer, int size, int count, int start) {
		throw new UnsupportedOperationException();
	}

	public void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected int compareData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected int signOfNonZeroAfter(int start) {
		throw new UnsupportedOperationException();
	}

	protected void addData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void subtractData(int myStart, PrimArithmeticArray other, int otherStart, int count) {
		throw new UnsupportedOperationException();
	}

	protected void printElementOn(int index, PrintStream oo) {
		throw new UnsupportedOperationException();
	}

	protected void copyElements(int to, PrimArray source, int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public int count() {
		return storage.length;
	}
}
