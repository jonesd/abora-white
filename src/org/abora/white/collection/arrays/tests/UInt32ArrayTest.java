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
import org.abora.white.collection.arrays.UInt32Array;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IEEE64Value;
import org.abora.white.value.IntegerValue;

public class UInt32ArrayTest extends TestCase {

	public static final long UINT32_MIN_VALUE = 0;
	public static final long UINT32_MAX_VALUE = 0xffffffffL;

	public UInt32ArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(UInt32ArrayTest.class);
	}

	public void testMakeCount() {
		UInt32Array array = UInt32Array.make(0);
		assertEquals(0, array.count());

		array = UInt32Array.make(1);
		AssertArrays.assertEquals(1, array.count());
		AssertArrays.assertEquals(0, array.uInt32At(0));
		
		try {
			UInt32Array.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testUInt32At() {
		UInt32Array a = UInt32Array.make(new long[] { 0, 1, 127, 255 });

		AssertArrays.assertEquals(0, a.uInt32At(0));
		AssertArrays.assertEquals(1, a.uInt32At(1));
		AssertArrays.assertEquals(127, a.uInt32At(2));
		AssertArrays.assertEquals(255, a.uInt32At(3));

		try {
			a.uInt32At(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.uInt32At(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testUInt32AtEmpty() {
		UInt32Array a = UInt32Array.make(0);
		try {
			a.uInt32At(0);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		UInt32Array a = UInt32Array.make(new long[] { 0, 1, 2, 3 });

		AssertArrays.assertEquals(IntegerValue.make(0), a.integerAt(0));
		AssertArrays.assertEquals(IntegerValue.make(1), a.integerAt(1));
		AssertArrays.assertEquals(IntegerValue.make(2), a.integerAt(2));
		AssertArrays.assertEquals(IntegerValue.make(3), a.integerAt(3));

		try {
			a.integerAt(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.integerAt(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testFetchValue() {
		UInt32Array a = UInt32Array.make(new long[] { 0, 1, 2, 3 });

		AssertArrays.assertEquals(0, ((IntegerValue) a.fetchValue(0)).asUInt8());
		AssertArrays.assertEquals(1, ((IntegerValue) a.fetchValue(1)).asUInt8());
		AssertArrays.assertEquals(2, ((IntegerValue) a.fetchValue(2)).asUInt8());
		AssertArrays.assertEquals(3, ((IntegerValue) a.fetchValue(3)).asUInt8());

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

	public void testCount() {
		AssertArrays.assertEquals(0, AssertArrays.makeUInt32ArrayEmpty().count());
		AssertArrays.assertEquals(1, UInt32Array.make(new long[] { 0 }).count());
		AssertArrays.assertEquals(2, UInt32Array.make(new long[] { 0, 1 }).count());
	}

	public void testStoreUInt8() {
		UInt32Array empty = UInt32Array.make(0);
		UInt32Array tri = UInt32Array.make(3);

		tri.storeUInt32(0, UINT32_MIN_VALUE);
		assertEquals(tri.uInt32At(0), UINT32_MIN_VALUE);
		tri.storeUInt32(1,  1);
		assertEquals(tri.uInt32At(1), 1);
		tri.storeUInt32(2, UINT32_MAX_VALUE);
		assertEquals(tri.uInt32At(2), UINT32_MAX_VALUE);

		try {
			tri.storeUInt32(-1,  1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeUInt32(3,  1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeUInt32(0,  1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		UInt32Array empty = UInt32Array.make(0);
		UInt32Array tri = UInt32Array.make(3);

		// Store integer values within spec
		tri.storeInteger(0, IntegerValue.make(UINT32_MIN_VALUE));
		assertTrue(tri.uInt32At(0) == UINT32_MIN_VALUE);
		tri.storeInteger(1, IntegerValue.make(1));
		assertEquals(tri.uInt32At(1), 1);
		tri.storeInteger(2, IntegerValue.make(UINT32_MAX_VALUE));
		assertTrue(tri.uInt32At(2) == UINT32_MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeInteger(0, IntegerValue.make(UINT32_MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeInteger(2, IntegerValue.make(UINT32_MAX_VALUE + 1));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			//expected
		}

		// Store outside array boundary
		try {
			tri.storeInteger(-1, IntegerValue.make(1));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeInteger(3, IntegerValue.make(1));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeInteger(0, IntegerValue.make(1));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreValue() {
		UInt32Array empty = UInt32Array.make(0);
		UInt32Array tri = UInt32Array.make(3);

		// Store integer values within spec
		tri.storeValue(0, IntegerValue.make(UINT32_MIN_VALUE));
		assertTrue(tri.uInt32At(0) == UINT32_MIN_VALUE);
		tri.storeValue(1, IntegerValue.make(1));
		assertEquals(tri.uInt32At(1), 1);
		tri.storeValue(2, IntegerValue.make(UINT32_MAX_VALUE));
		assertTrue(tri.uInt32At(2) == UINT32_MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeValue(0, IntegerValue.make(UINT32_MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeValue(2, IntegerValue.make(UINT32_MAX_VALUE + 1));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			// expected
		}

		// Store outside array boundary
		try {
			tri.storeValue(-1, IntegerValue.make(1));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeValue(3, IntegerValue.make(1));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeValue(0, IntegerValue.make(1));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		
		// non-compatible store
		try {
			tri.storeValue(0, IEEE64Value.make(1.1));
			fail("classCast");
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testStoreAll() {
		UInt32Array array = UInt32Array.make(0);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(UInt32Array.make(0), array);

		array = UInt32Array.make(1);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1 }), array);

		array = UInt32Array.make(3);
		array.storeAll(IntegerValue.make(2));
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 2, 2, 2 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(UInt32Array.make(new long[] { 1, 9, 9, 4, 5 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 0, 0, 4, 5 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeAll(IntegerValue.make(9), -1, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 9, 9, 9, 9 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeAll(IntegerValue.make(9), 2);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 9, 9, 3, 4, 5 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeAll(IntegerValue.make(9), 0, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2, 3, 4, 5 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeUInt32Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.storeAll(IntegerValue.make(UINT32_MAX_VALUE + 1), 1);
			fail();
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		UInt32Array array = AssertArrays.makeUInt32ArrayEmpty();
		array.storeMany(0, AssertArrays.makeUInt32ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeUInt32ArrayEmpty(), array);

		// simple
		array = AssertArrays.makeUInt32Array12345();
		array.storeMany(0, AssertArrays.makeUInt32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeUInt32Array12321(), array);

		array = AssertArrays.makeUInt32Array12345();
		array.storeMany(0, AssertArrays.makeUInt32Array12321());
		AssertArrays.assertEquals(AssertArrays.makeUInt32Array12321(), array);

		array = AssertArrays.makeUInt32Array12321();
		array.storeMany(1, UInt32Array.make(new long[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 5, 4, 2, 1 }), array);

		// Store compatible array
		array = AssertArrays.makeUInt32Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7 }));
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = AssertArrays.makeUInt32Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.storeMany(1, AssertArrays.makeUInt32Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.storeMany(0, AssertArrays.makeUInt32Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeUInt32Array12321();
		try {
			array.storeMany(1, Int64Array.make(new long[] { UINT32_MAX_VALUE + 1 }));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		UInt32Array array = AssertArrays.makeUInt32Array12345();
		long[] out = new long[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new long[] { 2, 3, 4 }));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new long[] { 1 }));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new long[] { 5 }));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new long[] { 1, 2, 3 }));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new long[] { 3, 4, 5 }));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new long[] { 4, 5, 0 }));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new long[] {
		}));

		array = AssertArrays.makeUInt32Array12345();
		out = new long[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeUInt32Array12345();
		out = new long[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		long[] charsE = new long[] {};
		long[] chars1 = new long[] { 1 };
		long[] chars12 = new long[] { 1, 2 };
		long[] chars11 = new long[] { 1, 1 };

		// Equal matches
		assertTrue(UInt32Array.make(charsE).isEqual(UInt32Array.make(charsE)));
		assertTrue(UInt32Array.make(chars1).isEqual(UInt32Array.make(chars1)));
		assertTrue(UInt32Array.make(chars12).isEqual(UInt32Array.make(chars12)));
		assertTrue(UInt32Array.make(chars1).isEqual(AssertArrays.makeInt32Array1()));
		
		// Unequal compatible matches
		assertFalse(UInt32Array.make(chars11).isEqual(UInt32Array.make(chars12)));
		assertFalse(UInt32Array.make(charsE).isEqual(UInt32Array.make(chars12)));
		assertFalse(UInt32Array.make(chars12).isEqual(UInt32Array.make(charsE)));

		// single value
		assertFalse(UInt32Array.make(chars1).isEqual(IntegerValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(UInt32Array.make(chars1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeUInt32ArrayEmpty().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array1().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt32Array1().indexOf(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12345().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt32Array12345().indexOf(IntegerValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12345().indexOf(IntegerValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12345().indexOf(IntegerValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeUInt32Array12321().indexOf(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeUInt32Array12321().indexOf(IEEE64Value.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = AssertArrays.makeUInt32ArrayEmpty().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array1().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array1().indexPast(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12345().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt32Array12345().indexPast(IntegerValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt32Array12345().indexPast(IntegerValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt32Array12345().indexPast(IntegerValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt32Array12345().indexPast(IntegerValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt32Array12345().indexPast(IntegerValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeUInt32Array12321().indexPast(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeUInt32Array12321().indexPast(IEEE64Value.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		UInt32Array array = AssertArrays.makeUInt32ArrayEmpty();
		UInt32Array search = AssertArrays.makeUInt32Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeUInt32ArrayEmpty();
		search = AssertArrays.makeUInt32ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeUInt32Array12345();
		//		search = makeUInt32ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = UInt32Array.make(new long[] { 1, 2, 3, 1, 2 });
		search = UInt32Array.make(new long[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeUInt32Array12321();
		search = UInt32Array.make(new long[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = UInt32Array.make(new long[] { 1, 2, 3, 1, 2 });
		Int32Array searchInt32 = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = UInt32Array.make(new long[] { 1, 2, 3, 1, 2 });
		search = UInt32Array.make(new long[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = UInt32Array.make(new long[] { 1, 1, 1 });
		search = UInt32Array.make(new long[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeUInt32Array12321();
		search = UInt32Array.make(new long[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeUInt32Array12321();
		search = UInt32Array.make(new long[] { 1, 2 });
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
		array = AssertArrays.makeUInt32Array12321();
		search = UInt32Array.make(new long[] { 1, 2 });
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
		array = UInt32Array.make(new long[] { 1, 2, 3, 1, 2 });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		UInt32Array array = AssertArrays.makeUInt32ArrayEmpty();
		array.addElements(0, AssertArrays.makeUInt32Array12321(), -1, 0);
		assertEquals(AssertArrays.makeUInt32ArrayEmpty(), array);

		array = AssertArrays.makeUInt32Array1();
		array.addElements(0, UInt32Array.make(new long[] { 9 }), -1, 0);
		assertEquals(UInt32Array.make(new long[] { 10 }), array);

		array = UInt32Array.make(5);
		array.addElements(0, AssertArrays.makeUInt32Array12321(), -1, 0);
		assertEquals(AssertArrays.makeUInt32Array12321(), array);

		array = AssertArrays.makeUInt32Array12345();
		array.addElements(0, AssertArrays.makeUInt32Array12321(), -1, 0);
		assertEquals(UInt32Array.make(new long[] { 2, 4, 6, 6, 6 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.addElements(2, AssertArrays.makeUInt32Array12321(), -1, 0);
		assertEquals(UInt32Array.make(new long[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.addElements(2, AssertArrays.makeUInt32Array12321(), -1);
		assertEquals(UInt32Array.make(new long[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.addElements(2, AssertArrays.makeUInt32Array12321());
		assertEquals(UInt32Array.make(new long[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.addElements(2, AssertArrays.makeUInt32Array12321(), 2, 1);
		assertEquals(UInt32Array.make(new long[] { 1, 2, 5, 7, 5 }), array);

		// element arithmetic overflows
		final long large = 0xa0000000L;
		array = UInt32Array.make(new long[] {large, 0});
		array.addElements(0, UInt32Array.make(new long[] {large, large}));
		//TODO check these values
		assertEquals(UInt32Array.make(new long[]{1073741824L, large}), array);

		array = AssertArrays.makeUInt32Array12345();
		try {
			array.addElements(2, AssertArrays.makeUInt32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeUInt32ArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		UInt32Array array = AssertArrays.makeUInt32ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeUInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeUInt32ArrayEmpty(), array);

		array = AssertArrays.makeUInt32Array1();
		array.subtractElements(0, UInt32Array.make(new long[] { 1 }), -1, 0);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 0 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.subtractElements(0, AssertArrays.makeUInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 0, 0, 0, 2, 4 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.subtractElements(2, AssertArrays.makeUInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.subtractElements(2, AssertArrays.makeUInt32Array12321(), -1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.subtractElements(2, AssertArrays.makeUInt32Array12321());
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt32Array12345();
		array.subtractElements(2, AssertArrays.makeUInt32Array12321(), 2, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2, 1, 1, 5 }), array);

		// element arithmetic overflows
		final long large = 0xa0000000L;
		array = UInt32Array.make(new long[] {0, large});
		array.subtractElements(0, UInt32Array.make(new long[] {large, large}));
		//TODO check these values
		assertEquals(UInt32Array.make(new long[]{1610612736L, 0}), array);

		// extend count beyond end of array
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeUInt32Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeUInt32ArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		UInt32Array array1 = AssertArrays.makeUInt32ArrayEmpty();
		UInt32Array array2 = AssertArrays.makeUInt32ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeUInt32Array12345();
		array2 = AssertArrays.makeUInt32Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeUInt32Array12321();
		array2 = AssertArrays.makeUInt32Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeUInt32Array12321();
		array2 = AssertArrays.makeUInt32Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeUInt32Array12345();
		array2 = AssertArrays.makeUInt32Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeUInt32ArrayEmpty();
		array2 = AssertArrays.makeUInt32Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeUInt32Array1();
		array2 = AssertArrays.makeUInt32ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = UInt32Array.make(new long[] { 0, 0 });
		array2 = UInt32Array.make(new long[] { 0 });
		assertEquals(0, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeUInt32Array12321();
		array2 = AssertArrays.makeUInt32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeUInt32Array12321();
		array2 = AssertArrays.makeUInt32Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeUInt32Array12321();
		array2 = AssertArrays.makeUInt32Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeUInt32Array12321();
		array2 = AssertArrays.makeUInt32Array12345();
		assertEquals(-1, array1.compare(array2, 10));

		// different array types
		array1 = AssertArrays.makeUInt32Array12345();
		Int32Array array2Int32 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeUInt32Array12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		UInt32Array array = AssertArrays.makeUInt32ArrayEmpty();
		UInt32Array copy = (UInt32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeUInt32ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeUInt32Array12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copyGrow(3);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		UInt32Array array = AssertArrays.makeUInt32ArrayEmpty();
		UInt32Array copy = (UInt32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeUInt32ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeUInt32Array12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeUInt32Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 0);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 1, 2 }), copy);

		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 3);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 4, 5 }), copy);

		// partial copy with too large count
		array = AssertArrays.makeUInt32Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeUInt32Array12345();
		copy = (UInt32Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(UInt32Array.make(new long[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeUInt32ArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", AssertArrays.makeUInt32Array12345().toString());
	}
}
