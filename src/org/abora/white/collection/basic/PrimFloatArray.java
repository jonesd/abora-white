/*
 * Abora hypertext system
 * Copyright 2003 David G Jones, david_jones@night.dircon.co.uk
 * 
 * Translated from Udanax-Gold source code: Copyright 1991 XOC, www.udanax.com
 */

package org.abora.white.collection.basic;

import java.io.PrintStream;

import org.abora.white.xpp.basic.Heaper;

public abstract class PrimFloatArray extends PrimDataArray {

//	/**
//	 * Constructor for PrimFloatArray.
//	 * @param count
//	 * @param datumSize
//	 */
//	protected PrimFloatArray(int count, int datumSize) {
//		super(count, datumSize);
//	}

	protected PrimFloatArray() {
		super();
	}

	/** Make an array initialized to zero values */
	public static PrimFloatArray zeros(int bitCount, int count) {
		throw new UnsupportedOperationException();
	}

	/** Store a floating point value */
	public void storeFloat(int index, double value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual floating point number */
	public double floatAt(int index) {
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

//	/** Return the maximum bits/entry that can be stored in this array */
//	public int bitCount() {
//		throw new UnsupportedOperationException();
//	}

	public int elementsHash(int count, int start) {
		throw new UnsupportedOperationException();
	}

	public int indexOf(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
	}

	public int indexPast(Heaper value, int start, int n) {
		throw new UnsupportedOperationException();
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

	protected void printElementOn(int index, PrintStream oo) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}
}
