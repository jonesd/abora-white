/*
 * Abora hypertext system
 * Copyright 2003 David G Jones, david_jones@night.dircon.co.uk
 * 
 * Translated from Udanax-Gold source code: Copyright 1991 XOC, www.udanax.com
 */

package org.abora.white.collection.basic;

import org.abora.gold.x.PrimSpec;


/**
 * @author jonesd
 */
public class SharedPtrArray extends PtrArray {

	private int myShareCount;

//	protected SharedPtrArray (Int32 count, TCSJ);

	protected SharedPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected SharedPtrArray(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	/** create a PtrArray filled with NULLs */
	public static SharedPtrArray make(int count) {
		throw new UnsupportedOperationException();
	}

	/** create a SharedPtrArray filled with the indicated data in 'from' */
	public static SharedPtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	public static SharedPtrArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static SharedPtrArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static SharedPtrArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create a PtrArray filled with the data from 'buffer' */
	public static SharedPtrArray make(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	public PrimSpec spec() {
		throw new UnsupportedOperationException();
	}

	public int shareCount() {
		throw new UnsupportedOperationException();
	}

	public void shareLess() {
		throw new UnsupportedOperationException();
	}

	public void shareMore() {
		throw new UnsupportedOperationException();
	}


	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}
}
