/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.arrays.tests;

import java.util.Arrays;

import junit.framework.TestCase;

import org.abora.white.collection.arrays.IEEE32Array;
import org.abora.white.collection.arrays.IEEE64Array;
import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.Int64Array;
import org.abora.white.collection.arrays.IntegerVarArray;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.exception.NotInTableException;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IEEE64Value;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class PtrArrayTest extends TestCase {

	public PtrArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(Int32ArrayTest.class);
	}

	public void testMakeCount() {
		PtrArray array = PtrArray.make(0);
		assertEquals(0, array.count());

		array = PtrArray.make(1);
		AssertArrays.assertEquals(1, array.count());
		AssertArrays.assertEquals(null, array.fetch(0));
		
		try {
			PtrArray.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testFetchValue() {
		PtrArray a = PtrArray.make(new Heaper[] { IntegerValue.one(), null, IEEE32Value.make(3.3f), PtrArray.make(2) });

		AssertArrays.assertEquals(IntegerValue.make(1), a.fetchValue(0));
		AssertArrays.assertEquals(null, a.fetchValue(1));
		AssertArrays.assertEquals(IEEE32Value.make(3.3f), a.fetchValue(2));
		AssertArrays.assertEquals(PtrArray.make(2), a.fetchValue(3));

		// Outside range
		try {
			a.fetchValue(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.fetchValue(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testGetValue() {
		PtrArray a = PtrArray.make(new Heaper[] { IntegerValue.one(), null, IEEE32Value.make(3.3f), PtrArray.make(2) });

		AssertArrays.assertEquals(IntegerValue.make(1), a.getValue(0));
		AssertArrays.assertEquals(IEEE32Value.make(3.3f), a.getValue(2));
		AssertArrays.assertEquals(PtrArray.make(2), a.getValue(3));

		// Fault null values
		try {
			a.getValue(1);
			fail("1");
		} catch (NotInTableException e) {
			// expected
		}

		// Outside range
		try {
			a.getValue(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.getValue(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testCount() {
		AssertArrays.assertEquals(0, PtrArray.make(0).count());
		AssertArrays.assertEquals(1, PtrArray.make(new Heaper[] { IntegerValue.zero() }).count());
		AssertArrays.assertEquals(2, PtrArray.make(new Heaper[] { IntegerValue.zero(), IntegerValue.one() }).count());
	}

	public void testStoreValue() {
		PtrArray empty = PtrArray.make(0);
		PtrArray tri = PtrArray.make(3);

		tri.storeValue(0, IntegerValue.one());
		assertEquals(tri.fetchValue(0), IntegerValue.one());
		tri.storeValue(1, null);
		assertEquals(tri.fetchValue(1), null);
		tri.storeValue(2, IntegerVarArray.make(3));
		assertEquals(tri.fetchValue(2), IntegerVarArray.make(3));
		assertFalse(tri.fetchValue(2).equals(IntegerVarArray.make(4)));

		// Outside range
		try {
			tri.storeValue(-1, IntegerValue.one());
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, IntegerValue.one());
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeValue(0, IntegerValue.one());
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreAll() {
		Int32Array array = Int32Array.make(0);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(Int32Array.make(0), array);

		array = Int32Array.make(1);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1 }), array);

		array = Int32Array.make(3);
		array.storeAll(IntegerValue.make(2));
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(Int32Array.make(new int[] { 1, 9, 9, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 0, 0, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(IntegerValue.make(9), -1, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 9, 9, 9, 9 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(IntegerValue.make(9), 2);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 9, 9, 3, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(IntegerValue.make(9), 0, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 3, 4, 5 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeAll(IntegerValue.make((long) Integer.MAX_VALUE + 1), 1);
			fail();
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		array.storeMany(0, AssertArrays.makeInt32ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		// simple
		array = AssertArrays.makeInt32Array12345();
		array.storeMany(0, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12321(), array);

		array = AssertArrays.makeInt32Array12345();
		array.storeMany(0, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12321(), array);

		array = AssertArrays.makeInt32Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 5, 4, 2, 1 }), array);

		array = AssertArrays.makeInt32Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7 }));
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = AssertArrays.makeInt32Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeMany(1, AssertArrays.makeInt32Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeInt32Array12345();
		try {
			array.storeMany(0, AssertArrays.makeInt32Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeInt32Array12321();
		try {
			array.storeMany(1, Int64Array.make(new long[] { (long)Integer.MAX_VALUE + 1 }));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		Int32Array array = AssertArrays.makeInt32Array12345();
		int[] out = new int[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new int[] { 2, 3, 4 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new int[] { 1 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new int[] { 5 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new int[] { 1, 2, 3 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new int[] { 3, 4, 5 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new int[] { 4, 5, 0 }));

		array = AssertArrays.makeInt32Array12345();
		out = new int[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new int[] {
		}));

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeInt32Array12345();
		out = new int[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		int[] intsE = new int[] {};
		int[] ints1 = new int[] { 1 };
		int[] ints12 = new int[] { 1, 2 };
		int[] ints11 = new int[] { 1, 1 };

		// Equal matches
		assertTrue(Int32Array.make(intsE).isEqual(Int32Array.make(intsE)));
		assertTrue(Int32Array.make(ints1).isEqual(Int32Array.make(ints1)));
		assertTrue(Int32Array.make(ints12).isEqual(Int32Array.make(ints12)));
		assertTrue(Int32Array.make(ints1).isEqual(AssertArrays.makeInt32Array1()));
		
		// Unequal compatible matches
		assertFalse(Int32Array.make(ints11).isEqual(Int32Array.make(ints12)));
		assertFalse(Int32Array.make(intsE).isEqual(Int32Array.make(ints12)));
		assertFalse(Int32Array.make(ints12).isEqual(Int32Array.make(intsE)));

		// single value
		assertFalse(Int32Array.make(ints1).isEqual(IntegerValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(Int32Array.make(ints1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeInt32ArrayEmpty().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array1().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array1().indexOf(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12345().indexOf(IntegerValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexOf(IntegerValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexOf(IntegerValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeInt32Array12321().indexOf(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeInt32Array12321().indexOf(IEEE64Value.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = AssertArrays.makeInt32ArrayEmpty().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array1().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array1().indexPast(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(IntegerValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12345().indexPast(IntegerValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt32Array12345().indexPast(IntegerValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12345().indexPast(IntegerValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12345().indexPast(IntegerValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeInt32Array12321().indexPast(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeInt32Array12321().indexPast(IEEE64Value.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		Int32Array search = AssertArrays.makeInt32Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeInt32ArrayEmpty();
		search = AssertArrays.makeInt32ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeInt32Array12345();
		//		search = makeInt32ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		search = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		Int32Array searchInt32 = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		search = Int32Array.make(new int[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = Int32Array.make(new int[] { 1, 1, 1 });
		search = Int32Array.make(new int[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 1, 2 });
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
		array = AssertArrays.makeInt32Array12321();
		search = Int32Array.make(new int[] { 1, 2 });
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

		// incompatible arrays
		array = Int32Array.make(new int[] { 1, 2, 3, 1, 2 });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		array = AssertArrays.makeInt32Array1();
		array.addElements(0, Int32Array.make(new int[] { 9 }), -1, 0);
		assertEquals(Int32Array.make(new int[] { 10 }), array);

		array = Int32Array.make(5);
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		assertEquals(AssertArrays.makeInt32Array12321(), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		assertEquals(Int32Array.make(new int[] { 2, 4, 6, 6, 6 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321(), -1, 0);
		assertEquals(Int32Array.make(new int[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321(), -1);
		assertEquals(Int32Array.make(new int[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321());
		assertEquals(Int32Array.make(new int[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.addElements(2, AssertArrays.makeInt32Array12321(), 2, 1);
		assertEquals(Int32Array.make(new int[] { 1, 2, 5, 7, 5 }), array);

		// element arithmetic overflows
		final int large = 2000000000;
		array = Int32Array.make(new int[] {large, -large, -large});
		array.addElements(0, Int32Array.make(new int[] {large, large, -large}));
		assertEquals(Int32Array.make(new int[]{-294967296, 0, 294967296}), array);

		// compatible array types
		array = AssertArrays.makeInt32Array12345();
		array.addElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[]{2, 4, 6, 6, 6}), array);

		array = AssertArrays.makeInt32Array12345();
		try {
			array.addElements(2, AssertArrays.makeInt32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeInt32ArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), array);

		array = AssertArrays.makeInt32Array1();
		array.subtractElements(0, Int32Array.make(new int[] { 9 }), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { -8 }), array);

		array = Int32Array.make(5);
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { -1, -2, -3, -2, -1 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 0, 0, 0, 2, 4 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321(), -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321());
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(2, AssertArrays.makeInt32Array12321(), 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 1, 1, 5 }), array);

		// element arithmetic overflows
		final int large = 2000000000;
		array = Int32Array.make(new int[] {large, large, -large});
		array.subtractElements(0, Int32Array.make(new int[] {-large, large, large}));
		assertEquals(Int32Array.make(new int[]{-294967296, 0, 294967296}), array);

		// compatible array types
		array = AssertArrays.makeInt32Array12345();
		array.subtractElements(0, AssertArrays.makeInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[]{0, 0, 0, 2, 4}), array);

		// extend count beyond end of array
		array = AssertArrays.makeInt32Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeInt32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeInt32ArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		Int32Array array1 = AssertArrays.makeInt32ArrayEmpty();
		Int32Array array2 = AssertArrays.makeInt32ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array12345();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array12345();
		array2 = AssertArrays.makeInt32Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeInt32ArrayEmpty();
		array2 = AssertArrays.makeInt32Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeInt32Array1();
		array2 = AssertArrays.makeInt32ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = Int32Array.make(new int[] { 0, 0 });
		array2 = Int32Array.make(new int[] { 0 });
		assertEquals(0, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeInt32Array12321();
		array2 = AssertArrays.makeInt32Array12345();
		assertEquals(-1, array1.compare(array2, 10));

		// compare near minimum held value
		array1 = Int32Array.make(new int[] { Integer.MIN_VALUE});
		array2 = Int32Array.make(new int[] { Integer.MAX_VALUE});
		assertEquals(-1, array1.compare(array2));

		// different array types
		array1 = AssertArrays.makeInt32Array12345();
		Int32Array array2Int32 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeInt32Array12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		Int32Array copy = (Int32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copyGrow(3);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		Int32Array array = AssertArrays.makeInt32ArrayEmpty();
		Int32Array copy = (Int32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeInt32ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 0);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 1, 2 }), copy);

		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 3);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 4, 5 }), copy);

		// partial copy with too large count
		array = AssertArrays.makeInt32Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeInt32Array12345();
		copy = (Int32Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeInt32ArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", AssertArrays.makeInt32Array12345().toString());
	}

	public void testZeroElements() {
		Int32Array array = Int32Array.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int32Array.make(0), array);
		
		// zero all elements
		array = Int32Array.make(1);
		array.storeInt32(0, 1);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {0}), array);

		array = AssertArrays.makeInt32Array12345();
		array.zeroElements();
		AssertArrays.assertEquals(Int32Array.make(new int[] {0, 0, 0, 0, 0}), array);
		
		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {0, 0, 0, 0, 0}), array);

		// zero subset of elements
		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 0, 0, 0, 0}), array);

		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 0, 0, 4, 5}), array);

		array = AssertArrays.makeInt32Array12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(Int32Array.make(new int[] {1, 2, 3, 4, 0}), array);

		// silently truncate from
		array = AssertArrays.makeInt32Array12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeInt32Array12345(), array);

		// extend count outside range
		array = AssertArrays.makeInt32Array12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
}
