/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.arrays.tests;

import java.util.Arrays;

import org.abora.white.collection.arrays.IEEE32Array;
import org.abora.white.collection.arrays.IEEE64Array;
import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.PrimFloatArray;
import org.abora.white.value.IntegerValue;
import org.abora.white.value.PrimFloatValue;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IEEE64Value;

import junit.framework.TestCase;

public class IEEE32ArrayTest extends TestCase {
	private static final float DIFF = 0.000001f;

	public IEEE32ArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(IEEE32ArrayTest.class);
	}

	protected IEEE32Array makeIEEE32ArrayEmpty() {
		return IEEE32Array.make(new float[] {});
	}
	protected IEEE32Array makeIEEE32Array1() {
		return IEEE32Array.make(new float[] {1.1f});
	}
	protected IEEE32Array makeIEEE32Array12345() {
		return IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 4.4f, 5.5f});
	}
	protected IEEE32Array makeIEEE32Array12321() {
		return IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 2.2f, 1.1f});
	}

	public void assertEquals(PrimFloatArray expected, PrimFloatArray actual, double diff) {
		assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			double expectedValue = expected.floatAt(i);
			double actualValue = actual.floatAt(i);
			assertEquals(expectedValue, actualValue, diff);
		}
	}

	public void testMakeCount() {
		IEEE32Array array = IEEE32Array.make(0);
		assertEquals(0, array.count());
		
		array = IEEE32Array.make(1);
		assertEquals(1, array.count());
		assertEquals(0.0f, array.iEEE32At(0), DIFF);
	}

	public void testIEEE32At() {
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, -2.2f, 3.3f });

		assertEquals(0.0f, a.iEEE32At(0), DIFF);
		assertEquals(1.1f, a.iEEE32At(1), DIFF);
		assertEquals(-2.2f, a.iEEE32At(2), DIFF);
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
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, -2.2f, 3.3f });

		assertEquals(0.0f, a.floatAt(0), DIFF);
		assertEquals(1.1f, a.floatAt(1), DIFF);
		assertEquals(-2.2f, a.floatAt(2), DIFF);
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
		IEEE32Array a = IEEE32Array.make(new float[] { 0.0f, 1.1f, -2.2f, 3.3f });

		assertEquals(0.0f, ((PrimFloatValue)a.fetchValue(0)).asIEEE32(), DIFF);
		assertEquals(1.1f, ((PrimFloatValue)a.fetchValue(1)).asIEEE32(), DIFF);
		assertEquals(-2.2f, ((PrimFloatValue)a.fetchValue(2)).asIEEE32(), DIFF);
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

		tri.storeValue(0, IEEE32Value.make(Float.MIN_VALUE));
		assertTrue(tri.iEEE32At(0) == Float.MIN_VALUE);
		tri.storeValue(1, IEEE32Value.make(1.1f));
		assertEquals(tri.iEEE32At(1), 1.1f, DIFF);
		tri.storeValue(2, IEEE32Value.make(Float.MAX_VALUE));
		assertTrue(tri.iEEE32At(2) == Float.MAX_VALUE);

		tri.storeValue(0, IEEE64Value.make(Double.MIN_VALUE));
		assertFalse(tri.iEEE32At(0) == Double.MIN_VALUE);
		tri.storeValue(0, IEEE64Value.make(1.1));
		assertTrue(tri.iEEE32At(0) == 1.1f);
		assertFalse(tri.iEEE32At(0) == 1.1);
		tri.storeValue(2, IEEE64Value.make(Double.MAX_VALUE));
		assertFalse(tri.iEEE32At(2) == Double.MAX_VALUE);
		
		try {
			tri.storeValue(-1, IEEE32Value.make(1.1f));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, IEEE32Value.make(1.1f));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		try {
			empty.storeValue(0, IEEE32Value.make(1.1f));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreAll() {
		IEEE32Array array = IEEE32Array.make(0);
		array.storeAll(IEEE32Value.make(1.1f));
		assertEquals(IEEE32Array.make(0), array, DIFF);
		
		array = IEEE32Array.make(1);
		array.storeAll(IEEE32Value.make(1.1f));
		assertEquals(IEEE32Array.make(new float[] {1.1f}), array, DIFF);
		
		array = IEEE32Array.make(3);
		array.storeAll(IEEE32Value.make(2.2f));
		assertEquals(IEEE32Array.make(new float[] {2.2f, 2.2f, 2.2f}), array, DIFF);
		
		array = makeIEEE32Array12345();
		array.storeAll(IEEE32Value.make(9.9f), 2, 1);
		assertEquals(IEEE32Array.make(new float[] {1.1f, 9.9f, 9.9f, 4.4f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.storeAll(null, 2, 1);
		assertEquals(IEEE32Array.make(new float[] {1.1f, 0.0f, 0.0f, 4.4f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.storeAll(IEEE32Value.make(9.9f), -1, 1);
		assertEquals(IEEE32Array.make(new float[] {1.1f, 9.9f, 9.9f, 9.9f, 9.9f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.storeAll(IEEE32Value.make(9.9f), 2);
		assertEquals(IEEE32Array.make(new float[] {9.9f, 9.9f, 3.3f, 4.4f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.storeAll(IEEE32Value.make(9.9f), 0, 1);
		assertEquals(IEEE32Array.make(new float[] {1.1f, 2.2f, 3.3f, 4.4f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.storeAll(IEEE64Value.make(9.9), 2, 1);
		assertEquals(IEEE32Array.make(new float[] {1.1f, 9.9f, 9.9f, 4.4f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = makeIEEE32Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = makeIEEE32Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testStoreMany() {
		// empty
		IEEE32Array array = makeIEEE32ArrayEmpty();
		array.storeMany(0, makeIEEE32ArrayEmpty());
		assertEquals(makeIEEE32ArrayEmpty(), array, DIFF);

		// simple
		array = makeIEEE32Array12345();
		array.storeMany(0, makeIEEE32Array12321());
		assertEquals(makeIEEE32Array12321(), array, DIFF);

		array = makeIEEE32Array12345();
		array.storeMany(0, makeIEEE32Array12321());
		assertEquals(makeIEEE32Array12321(), array, DIFF);

		array = makeIEEE32Array12321();
		array.storeMany(1, IEEE32Array.make(new float[]{8.8f, 7.7f, 6.6f, 5.5f, 4.4f, 3,3f}), 2, 3);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 5.5f, 4.4f, 2.2f, 1.1f}), array, DIFF);

		// copy different types of array
		array = makeIEEE32Array12321();
		array.storeMany(1, IEEE64Array.make(new double[]{8.8, 7.7}));
		assertEquals(IEEE32Array.make(new float[]{1.1f, 8.8f, 7.7f, 2.2f, 1.1f}), array, DIFF);

		array = makeIEEE32Array12321();
		try {
			array.storeMany(1, Int32Array.make(new int[]{8, 7}));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = makeIEEE32Array12345();
		try {
			array.storeMany(1, makeIEEE32Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = makeIEEE32Array12345();
		try {
			array.storeMany(0, makeIEEE32Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}
	}
	
	public void testCopyToBuffer() {
		IEEE32Array array = makeIEEE32Array12345();
		float[] out = new float[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new float[] {2.2f, 3.3f, 4.4f}));		

		array = makeIEEE32Array12345();
		out = new float[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new float[] {1.1f}));		

		array = makeIEEE32Array12345();
		out = new float[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new float[] {5.5f}));		

		array = makeIEEE32Array12345();
		out = new float[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new float[] {1.1f, 2.2f, 3.3f}));		

		array = makeIEEE32Array12345();
		out = new float[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new float[] {3.3f, 4.4f, 5.5f}));		

		array = makeIEEE32Array12345();
		out = new float[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new float[] {4.4f, 5.5f, 0.0f}));		

		array = makeIEEE32Array12345();
		out = new float[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new float[] {}));		

		array = makeIEEE32Array12345();
		out = new float[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = makeIEEE32Array12345();
		out = new float[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
	}
	
	public void testIsEqual() {
		float[] floatE = new float[] {};
		float[] float1 = new float[] {1.1f};
		float[] float12 = new float[] {1.1f, 2.2f};
		float[] float11 = new float[] {1.1f, 1.1f};
		
		assertTrue(IEEE32Array.make(floatE).isEqual(IEEE32Array.make(floatE)));
		assertTrue(IEEE32Array.make(float1).isEqual(IEEE32Array.make(float1)));
		assertTrue(IEEE32Array.make(float12).isEqual(IEEE32Array.make(float12)));
		assertFalse(IEEE32Array.make(float11).isEqual(IEEE32Array.make(float12)));
		assertFalse(IEEE32Array.make(floatE).isEqual(IEEE32Array.make(float12)));
		assertFalse(IEEE32Array.make(float1).isEqual(IEEE32Value.make(1.1f)));
		assertFalse(IEEE32Array.make(float12).isEqual(IEEE32Array.make(floatE)));
	}
	
	public void testIndexOf() {		
		int index = makeIEEE32ArrayEmpty().indexOf(IEEE32Value.make(1.1f), 0, 1);
		assertEquals(-1, index);

		index = makeIEEE32Array1().indexOf(IEEE32Value.make(1.1f), 0, 1);
		assertEquals(0, index);

		index = makeIEEE32Array1().indexOf(IEEE32Value.make(1.1f), 0, 0);
		assertEquals(-1, index);
		
		index = makeIEEE32Array12345().indexOf(IEEE32Value.make(1.1f), 0, 1);
		assertEquals(0, index);

		index = makeIEEE32Array12345().indexOf(IEEE32Value.make(1.1f), 0, 2);
		assertEquals(-1, index);

		index = makeIEEE32Array12345().indexOf(IEEE32Value.make(1.1f), 1, 1);
		assertEquals(-1, index);

		index = makeIEEE32Array12345().indexOf(IEEE32Value.make(5.5f), 0, 1);
		assertEquals(4, index);
		
		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), 0, 1);
		assertEquals(1, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), 0, 2);
		assertEquals(3, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(1.1f), -1, -1);
		assertEquals(4, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -1, -1);
		assertEquals(3, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -1, 1);
		assertEquals(-1, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -1, -2);
		assertEquals(1, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -1, -3);
		assertEquals(-1, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -3, -1);
		assertEquals(1, index);

		index = makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -1, 1);
		assertEquals(-1, index);

		try {
			makeIEEE32Array12321().indexOf(IEEE32Value.make(2.2f), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			makeIEEE32Array12321().indexOf(IntegerValue.make(2), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testIndexPast() {		
		int index = makeIEEE32ArrayEmpty().indexPast(IEEE32Value.make(1.1f), 0, 1);
		assertEquals(-1, index);

		index = makeIEEE32Array1().indexPast(IEEE32Value.make(1.1f), 0, 1);
		assertEquals(-1, index);

		index = makeIEEE32Array1().indexPast(IEEE32Value.make(1.1f), 0, 0);
		assertEquals(-1, index);
		
		index = makeIEEE32Array12345().indexPast(IEEE32Value.make(1.1f), 0, 1);
		assertEquals(1, index);

		index = makeIEEE32Array12345().indexPast(IEEE32Value.make(1.1f), 0, 2);
		assertEquals(2, index);

		index = makeIEEE32Array12345().indexPast(IEEE32Value.make(1.1f), 1, 1);
		assertEquals(1, index);

		index = makeIEEE32Array12345().indexPast(IEEE32Value.make(5.5f), 0, 1);
		assertEquals(0, index);

		index = makeIEEE32Array12345().indexPast(IEEE32Value.make(5.5f), 3, 1);
		assertEquals(3, index);

		index = makeIEEE32Array12345().indexPast(IEEE32Value.make(5.5f), 4, 1);
		assertEquals(-1, index);
		
		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), 0, 1);
		assertEquals(0, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), 0, 2);
		assertEquals(2, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(1.1f), -1, -1);
		assertEquals(3, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -1, -1);
		assertEquals(4, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -1, 1);
		assertEquals(4, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -1, 2);
		assertEquals(-1, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -1, -2);
		assertEquals(2, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -1, -3);
		assertEquals(0, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -3, -1);
		assertEquals(2, index);

		index = makeIEEE32Array12321().indexPast(IEEE32Value.make(1.1f), -1, 1);
		assertEquals(-1, index);

		try {
			makeIEEE32Array12321().indexPast(IEEE32Value.make(2.2f), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testIndexOfElements() {
		// empty
		IEEE32Array array = makeIEEE32ArrayEmpty();
		IEEE32Array search = makeIEEE32Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = makeIEEE32ArrayEmpty();
		search = makeIEEE32ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

// TODO skip zero length other?
//		array = makeIEEE32Array12345();
//		search = makeIEEE32ArrayEmpty();
//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = IEEE32Array.make(new float[]{1.1f, 2.2f, 3.3f, 1.1f, 2.2f});
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{4.4f, 9.9f, 2.2f, 8.8f});
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));
		
		// reverse search		
		array = IEEE32Array.make(new float[]{1.1f, 2.2f, 3.3f, 1.1f, 2.2f});
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = IEEE32Array.make(new float[]{1.1f, 1.1f, 1.1f});
		search = IEEE32Array.make(new float[]{1.1f, 1.1f});
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{1.1f});
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
		try {
			array.indexOfElements(search, 3);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			array.indexOfElements(search, 2, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// invalid start		
		array = makeIEEE32Array12321();
		search = IEEE32Array.make(new float[]{1.1f, 2.2f});
// TODO different invalid start values if + or - 
//		try {
//			array.indexOfElements(search, -1, 0, 6, 0);
//			fail();
//		} catch (IndexOutOfBoundsException e) {
//			// expected
//		}
		try {
			array.indexOfElements(search, -1, 0, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}		
	}
	
	public void testAddElements() {
		IEEE32Array array = makeIEEE32ArrayEmpty();
		array.addElements(0, makeIEEE32Array12321(), -1, 0);
		assertEquals(makeIEEE32ArrayEmpty(), array, DIFF);

		array = makeIEEE32Array1();
		array.addElements(0, IEEE32Array.make(new float[]{9f}), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{10.1f}), array, DIFF);

		array = IEEE32Array.make(5);
		array.addElements(0, makeIEEE32Array12321(), -1, 0);
		assertEquals(makeIEEE32Array12321(), array, DIFF);

		array = makeIEEE32Array12345();
		array.addElements(0, makeIEEE32Array12321(), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{2.2f, 4.4f, 6.6f, 6.6f, 6.6f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.addElements(2, makeIEEE32Array12321(), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 4.4f, 6.6f, 8.8f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.addElements(2, makeIEEE32Array12321(), -1);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 4.4f, 6.6f, 8.8f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.addElements(2, makeIEEE32Array12321());
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 4.4f, 6.6f, 8.8f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.addElements(2, makeIEEE32Array12321(), 2, 1);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 5.5f, 7.7f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		try {
			array.addElements(2, makeIEEE32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		IEEE32Array array = makeIEEE32ArrayEmpty();
		array.subtractElements(0, makeIEEE32Array12321(), -1, 0);
		assertEquals(makeIEEE32ArrayEmpty(), array, DIFF);

		array = makeIEEE32Array1();
		array.subtractElements(0, IEEE32Array.make(new float[]{9f}), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{-7.9f}), array, DIFF);

		array = IEEE32Array.make(5);
		array.subtractElements(0, makeIEEE32Array12321(), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{-1.1f, -2.2f, -3.3f, -2.2f, -1.1f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.subtractElements(0, makeIEEE32Array12321(), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{0.0f, 0.0f, 0.0f, 2.2f, 4.4f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.subtractElements(2, makeIEEE32Array12321(), -1, 0);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 2.2f, 2.2f, 2.2f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.subtractElements(2, makeIEEE32Array12321(), -1);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 2.2f, 2.2f, 2.2f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.subtractElements(2, makeIEEE32Array12321());
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 2.2f, 2.2f, 2.2f}), array, DIFF);

		array = makeIEEE32Array12345();
		array.subtractElements(2, makeIEEE32Array12321(), 2, 1);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 1.1f, 1.1f, 5.5f}), array, DIFF);

		array = makeIEEE32Array12345();
		try {
			array.subtractElements(2, makeIEEE32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		IEEE32Array array1 = makeIEEE32ArrayEmpty();
		IEEE32Array array2 = makeIEEE32ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = makeIEEE32Array12345();
		array2 = makeIEEE32Array12345();
		assertEquals(0, array1.compare(array2));
		
		array1 = makeIEEE32Array12321();
		array2 = makeIEEE32Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = makeIEEE32Array12321();
		array2 = makeIEEE32Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = makeIEEE32Array12345();
		array2 = makeIEEE32Array12321();
		assertEquals(1, array1.compare(array2));
		
		// auto-filling with 0
		array1 = makeIEEE32ArrayEmpty();
		array2 = makeIEEE32Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = makeIEEE32Array1();
		array2 = makeIEEE32ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = IEEE32Array.make(new float[]{0.0f, 0.0f});
		array2 = IEEE32Array.make(new float[]{0.0f});
		assertEquals(0, array1.compare(array2));

		// compare sub-regions		
		array1 = makeIEEE32Array12321();
		array2 = makeIEEE32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = makeIEEE32Array12321();
		array2 = makeIEEE32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = makeIEEE32Array12321();
		array2 = makeIEEE32Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = makeIEEE32Array12321();
		array2 = makeIEEE32Array12345();
		assertEquals(-1, array1.compare(array2, 10));
	}
	
	public void testCopyGrow() {
		IEEE32Array array = makeIEEE32ArrayEmpty();
		IEEE32Array copy = (IEEE32Array) array.copyGrow(0);
		assertEquals(makeIEEE32ArrayEmpty(), copy, DIFF);
		assertNotSame(array, copy);
		
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copyGrow(0);
		assertEquals(makeIEEE32Array12345(), copy, DIFF);
		assertNotSame(array, copy);

		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copyGrow(3);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f, 3.3f, 4.4f, 5.5f, 0.0f, 0.0f, 0.0f}), copy, DIFF);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		IEEE32Array array = makeIEEE32ArrayEmpty();
		IEEE32Array copy = (IEEE32Array) array.copy();
		assertEquals(makeIEEE32ArrayEmpty(), copy, DIFF);
		assertNotSame(array, copy);
		
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy();
		assertEquals(makeIEEE32Array12345(), copy, DIFF);
		assertEquals(makeIEEE32Array12345(), array, DIFF);
		assertNotSame(array, copy);
		
		// partial copy
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1);
		assertEquals(IEEE32Array.make(new float[]{2.2f, 3.3f}), copy, DIFF);
		assertNotSame(array, copy);

		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 0);
		assertEquals(IEEE32Array.make(new float[]{1.1f, 2.2f}), copy, DIFF);

		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 3);
		assertEquals(IEEE32Array.make(new float[]{4.4f, 5.5f}), copy, DIFF);

		// partial copy with too large count
		array = makeIEEE32Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1);
		assertEquals(IEEE32Array.make(new float[]{2.2f, 3.3f}), copy, DIFF);

		// partial copy with leading space
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1, 1);
		assertEquals(IEEE32Array.make(new float[]{0.0f, 2.2f, 3.3f}), copy, DIFF);

		// partial copy with leading space
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1, 0, 1);
		assertEquals(IEEE32Array.make(new float[]{2.2f, 3.3f, 0.0f}), copy, DIFF);

		// partial copy with leading and trailing space
		array = makeIEEE32Array12345();
		copy = (IEEE32Array) array.copy(2, 1, 2, 1);
		assertEquals(IEEE32Array.make(new float[]{0.0f, 0.0f, 2.2f, 3.3f, 0.0f}), copy, DIFF);
		
	}
}

