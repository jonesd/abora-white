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

import org.abora.white.value.PrimSpec;
import org.abora.white.xpp.basic.Heaper;

public class SharedPtrArray extends PtrArray {
	private int myShareCount = 0;

	//////////////////////////////////////////////
	// Constructors

	protected SharedPtrArray(int count) {
		super(count);
	}

	protected SharedPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(size, from, sourceOffset, count, destOffset);
	}

	protected SharedPtrArray(Heaper[] buffer) {
		super(buffer);
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/** create a PtrArray filled with NULLs */
	public static SharedPtrArray make(int count) {
		return new SharedPtrArray(count);
	}

	/** create a SharedPtrArray filled with the indicated data in 'from' */
	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		return new SharedPtrArray(size, from, sourceOffset, count, destOffset);
	}

	public static PtrArray make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static PtrArray make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static PtrArray make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	/** create a PtrArray filled with the data from 'buffer' */
	public static PtrArray make(Heaper[] buffer) {
		return new SharedPtrArray(buffer);
	}

	protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
		return make(size, (PtrArray) source, sourceOffset, count, destOffset);
	}

	//////////////////////////////////////////////
	// Accessing

	public PrimSpec spec() {
		return PrimSpec.sharedPointer();
	}

	public int shareCount() {
		return myShareCount;
		//		INLINE Int4 SharedPtrArray::shareCount () {
		//			return myShareCount;
	}

	public void shareLess() {
		myShareCount -= 1;
		//		void SharedPtrArray::shareLess () {
		//			myShareCount -= 1;
		//		}
	}

	public void shareMore() {
		myShareCount += 1;
		//		void SharedPtrArray::shareMore () {
		//			myShareCount += 1;
		//		}
	}
}
