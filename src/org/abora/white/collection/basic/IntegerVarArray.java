/*
 * Abora hypertext system
 * Copyright 2003 David G Jones, david_jones@night.dircon.co.uk
 * 
 * Translated from Udanax-Gold source code: Copyright 1991 XOC, www.udanax.com
 */

package org.abora.white.collection.basic;

import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Xmtr;
import org.abora.gold.xpp.basic.Heaper;

/**
 * @author jonesd
 */
public class IntegerVarArray extends PrimIntegerArray {

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
	public void storeIntegerVar(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

	/** Get an actual integer value */
	public IntegerVar integerVarAt (int index) {
		throw new UnsupportedOperationException();
	}

	public void storeInteger(int index, IntegerVar value) {
		throw new UnsupportedOperationException();
	}

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

		public void copyToBuffer(int[] buffer, int size, int count, int start) {
			throw new UnsupportedOperationException();
		}

	public void zeroElements(int from, int count) {
		throw new UnsupportedOperationException();
	}


	private void receiveIVArray(Rcvr rcvr) {
		throw new UnsupportedOperationException();
	}

	private void sendIVArray(Xmtr xmtr) {
		throw new UnsupportedOperationException();
	}

	/**
	 * Only called in construction of fresh array when there is no possibility
	 * of storage leak from skipping IntegerVar assignment. Also, the normal
	 * zeroElements is itself unsafe during construction, because the IntegerVar
	 * code could interpret the random bits in the newly allocated
	 * IntegerVarArray as pointers to bignums to be freed.
	 */
	protected void unsafeZeroElements(int from, int count) {
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

	protected void copyElements(int to, PrimArray source, int from, int count) {
		throw new UnsupportedOperationException();
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

}
