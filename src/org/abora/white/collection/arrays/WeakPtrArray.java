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

//TODO need to know more about the implementation
//TODO should this extend SharedPtrArray
public class WeakPtrArray extends PtrArray {

	protected WeakPtrArray(int count, int[] buffer) {
		super(count, buffer);
		throw new UnsupportedOperationException();
	}

}
