/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.arrays.tests;

import org.abora.white.collection.arrays.IEEE32Array;
import org.abora.white.x.PrimFloatValue;
import org.abora.white.x.PrimIEEE32;
import org.abora.white.x.PrimIEEE64;

import junit.framework.TestCase;

public class IEEE32ArrayTest extends TestCase {
	private static final float DIFF = 0f;

	public IEEE32ArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(IEEE32ArrayTest.class);
	}

	public void testIEEE32At() {
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, 2.2f, 3.3f });

		assertEquals(0.0f, a.iEEE32At(0), DIFF);
		assertEquals(1.1f, a.iEEE32At(1), DIFF);
		assertEquals(2.2f, a.iEEE32At(2), DIFF);
		assertEquals(3.3f, a.iEEE32At(3), DIFF);

		try {
			a.iEEE32At(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.iEEE32At(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testIEEE32AtEmpty() {
		IEEE32Array a = IEEE32Array.make(0);
		try {
			a.iEEE32At(0);
			fail("expected 0 failure");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}
	
	public void testFloatAt() {
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, 2.2f, 3.3f });

		assertEquals(0.0f, a.floatAt(0), DIFF);
		assertEquals(1.1f, a.floatAt(1), DIFF);
		assertEquals(2.2f, a.floatAt(2), DIFF);
		assertEquals(3.3f, a.floatAt(3), DIFF);

		try {
			a.floatAt(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.floatAt(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testFetchValue() {
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, 2.2f, 3.3f });

		assertEquals(0.0f, ((PrimFloatValue)a.fetchValue(0)).asIEEE32(), DIFF);
		assertEquals(1.1f, ((PrimFloatValue)a.fetchValue(1)).asIEEE32(), DIFF);
		assertEquals(2.2f, ((PrimFloatValue)a.fetchValue(2)).asIEEE32(), DIFF);
		assertEquals(3.3f, ((PrimFloatValue)a.fetchValue(3)).asIEEE32(), DIFF);

		try {
			a.fetchValue(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.fetchValue(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testCount() {
		assertEquals(IEEE32Array.make(new float[] {}).count(), 0); 
		assertEquals(IEEE32Array.make(new float[] {0.0f}).count(), 1);
		assertEquals(IEEE32Array.make(new float[] {0.0f, 1.1f}).count(), 2);  
	}
	
	public void testStoreIEEE32() {
		IEEE32Array empty = IEEE32Array.make(0);
		IEEE32Array tri = IEEE32Array.make(3);

		tri.storeIEEE32(0, Float.MIN_VALUE);
		assertEquals(tri.iEEE32At(0), Float.MIN_VALUE, DIFF);
		tri.storeIEEE32(1, 1.1f);
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeIEEE32(2, Float.MAX_VALUE);
		assertEquals(tri.iEEE32At(2), Float.MAX_VALUE, DIFF);
		
		try {
			tri.storeIEEE32(-1, 1.1f);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeIEEE32(3, 1.1f);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeIEEE32(0, 1.1f);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreFloat() {
		IEEE32Array empty = IEEE32Array.make(0);
		IEEE32Array tri = IEEE32Array.make(3);

		tri.storeFloat(0, Float.MIN_VALUE);
		assertTrue(tri.iEEE32At(0) == Float.MIN_VALUE);
		tri.storeFloat(1, 1.1f);
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeFloat(2, Float.MAX_VALUE);
		assertTrue(tri.iEEE32At(2) == Float.MAX_VALUE);

		tri.storeFloat(0, Double.MIN_VALUE);
		assertFalse(tri.iEEE32At(0) == Double.MIN_VALUE);
		tri.storeFloat(1, 1.1d);
		assertTrue(tri.iEEE32At(1) == 1.1f);
		assertFalse(tri.iEEE32At(1) == 1.1);
		tri.storeFloat(2, Double.MAX_VALUE);
		assertFalse(tri.iEEE32At(2) == Double.MAX_VALUE);
		
		try {
			tri.storeFloat(-1, 1.1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeFloat(3, 1.1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeFloat(0, 1.1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreValue() {
		IEEE32Array empty = IEEE32Array.make(0);
		IEEE32Array tri = IEEE32Array.make(3);

		tri.storeValue(0, PrimIEEE32.make(Float.MIN_VALUE));
		assertTrue(tri.iEEE32At(0) == Float.MIN_VALUE);
		tri.storeValue(1, PrimIEEE32.make(1.1f));
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeValue(2, PrimIEEE32.make(Float.MAX_VALUE));
		assertTrue(tri.iEEE32At(2) == Float.MAX_VALUE);

		tri.storeValue(0, PrimIEEE64.make(Double.MIN_VALUE));
		assertFalse(tri.iEEE32At(0) == Double.MIN_VALUE);
		tri.storeValue(0, PrimIEEE64.make(1.1));
		assertTrue(tri.iEEE32At(0) == 1.1f);
		assertFalse(tri.iEEE32At(0) == 1.1);
		tri.storeValue(2, PrimIEEE64.make(Double.MAX_VALUE));
		assertFalse(tri.iEEE32At(2) == Double.MAX_VALUE);
		
		try {
			tri.storeValue(-1, PrimIEEE32.make(1.1f));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, PrimIEEE32.make(1.1f));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeValue(0, PrimIEEE32.make(1.1f));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreAll() {
		IEEE32Array array = IEEE32Array.make(1);
		array.storeAll(PrimIEEE32.make(1.1f));
		assertTrue(IEEE32Array.make(new float[] {1.1f}).isEqual(array));
		
		array = IEEE32Array.make(3);
		array.storeAll(PrimIEEE32.make(2.2f));
		assertTrue(IEEE32Array.make(new float[] {2.2f, 2.2f, 2.2f}).isEqual(array));
	}
	
	public void assertEquals(float[] expected, float[] actual) {
		assertEquals(expected.length, actual.length);
		for (int i = 0; i < expected.length; i++) {
			assertTrue(expected[i] == actual[i]);
		}
	}
}

