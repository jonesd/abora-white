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
package org.abora.white.collection.arrays.tests;

import junit.framework.Assert;

import org.abora.white.collection.arrays.IEEE32Array;
import org.abora.white.collection.arrays.IEEE64Array;
import org.abora.white.collection.arrays.Int16Array;
import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.Int64Array;
import org.abora.white.collection.arrays.Int8Array;
import org.abora.white.collection.arrays.PrimFloatArray;
import org.abora.white.collection.arrays.PrimIntegerArray;
import org.abora.white.value.IntegerValue;

public class AssertArrays extends Assert {

	/** Class not instantiable */
	private AssertArrays() {
		super();
	}

	//////////////////////////////////////////////
	// Asserts

	public static void assertEquals(PrimIntegerArray expected, PrimIntegerArray actual) {
		assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			IntegerValue expectedValue = expected.integerAt(i);
			IntegerValue actualValue = actual.integerAt(i);
			assertEquals(expectedValue, actualValue);
		}
	}

	public static void assertEquals(PrimFloatArray expected, PrimFloatArray actual, double diff) {
		assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			double expectedValue = expected.floatAt(i);
			double actualValue = actual.floatAt(i);
			assertEquals(expectedValue, actualValue, diff);
		}
	}

	//////////////////////////////////////////////
	// Int8
	
	protected static Int8Array makeInt8ArrayEmpty() {
		return Int8Array.make(new byte[] {
		});
	}
	protected static Int8Array makeInt8Array1() {
		return Int8Array.make(new byte[] { 1 });
	}
	protected static Int8Array makeInt8Array12345() {
		return Int8Array.make(new byte[] { 1, 2, 3, 4, 5 });
	}
	protected static Int8Array makeInt8Array12321() {
		return Int8Array.make(new byte[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// Int16
	
	protected static Int16Array makeInt16ArrayEmpty() {
		return Int16Array.make(new short[] {
		});
	}
	protected static Int16Array makeInt16Array1() {
		return Int16Array.make(new short[] { 1 });
	}
	protected static Int16Array makeInt16Array12345() {
		return Int16Array.make(new short[] { 1, 2, 3, 4, 5 });
	}
	protected static Int16Array makeInt16Array12321() {
		return Int16Array.make(new short[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// Int32
	
	protected static Int32Array makeInt32ArrayEmpty() {
		return Int32Array.make(new int[] {
		});
	}
	protected static Int32Array makeInt32Array1() {
		return Int32Array.make(new int[] { 1 });
	}
	protected static Int32Array makeInt32Array12345() {
		return Int32Array.make(new int[] { 1, 2, 3, 4, 5 });
	}
	protected static Int32Array makeInt32Array12321() {
		return Int32Array.make(new int[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// Int64
	
	protected static Int64Array makeInt64ArrayEmpty() {
		return Int64Array.make(new long[] {
		});
	}
	protected static Int64Array makeInt64Array1() {
		return Int64Array.make(new long[] { 1 });
	}
	protected static Int64Array makeInt64Array12345() {
		return Int64Array.make(new long[] { 1, 2, 3, 4, 5 });
	}
	protected static Int64Array makeInt64Array12321() {
		return Int64Array.make(new long[] { 1, 2, 3, 2, 1 });
	}

	//////////////////////////////////////////////
	// IEEE32
	
	protected static IEEE32Array makeIEEE32ArrayEmpty() {
		return IEEE32Array.make(new float[] {});
	}
	protected static IEEE32Array makeIEEE32Array1() {
		return IEEE32Array.make(new float[] {1.1f});
	}
	protected static IEEE32Array makeIEEE32Array12345() {
		return IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 4.4f, 5.5f});
	}
	protected static IEEE32Array makeIEEE32Array12321() {
		return IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 2.2f, 1.1f});
	}

	//////////////////////////////////////////////
	// IEEE64
	
	protected static IEEE64Array makeIEEE64ArrayEmpty() {
		return IEEE64Array.make(new double[] {});
	}
	protected static IEEE64Array makeIEEE64Array1() {
		return IEEE64Array.make(new double[] {1.1});
	}
	protected static IEEE64Array makeIEEE64Array12345() {
		return IEEE64Array.make(new double[] {1.1, 2.2, 3.3, 4.4, 5.5});
	}
	protected static IEEE64Array makeIEEE64Array12321() {
		return IEEE64Array.make(new double[] {1.1, 2.2, 3.3, 2.2, 1.1});
	}

}
