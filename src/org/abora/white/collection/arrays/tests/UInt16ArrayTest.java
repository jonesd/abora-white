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
import org.abora.white.collection.arrays.UInt16Array;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IEEE64Value;
import org.abora.white.value.IntegerValue;

public class UInt16ArrayTest extends TestCase {

	public UInt16ArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(UInt16ArrayTest.class);
	}

	public void testMakeCount() {
		UInt16Array array = UInt16Array.make(0);
		assertEquals(0, array.count());

		array = UInt16Array.make(1);
		AssertArrays.assertEquals(1, array.count());
		AssertArrays.assertEquals(0, array.uInt16At(0));
		
		try {
			UInt16Array.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testUInt16At() {
		UInt16Array a = UInt16Array.make(new char[] { 0, 1, 2, 3 });

		AssertArrays.assertEquals(0, a.uInt16At(0));
		AssertArrays.assertEquals(1, a.uInt16At(1));
		AssertArrays.assertEquals(2, a.uInt16At(2));
		AssertArrays.assertEquals(3, a.uInt16At(3));

		try {
			a.uInt16At(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.uInt16At(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testUInt16AtEmpty() {
		UInt16Array a = UInt16Array.make(0);
		try {
			a.uInt16At(0);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		UInt16Array a = UInt16Array.make(new char[] { 0, 1, 2, 3 });

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
		UInt16Array a = UInt16Array.make(new char[] { 0, 1, 2, 3 });

		AssertArrays.assertEquals(0, ((IntegerValue) a.fetchValue(0)).asUInt16());
		AssertArrays.assertEquals(1, ((IntegerValue) a.fetchValue(1)).asUInt16());
		AssertArrays.assertEquals(2, ((IntegerValue) a.fetchValue(2)).asUInt16());
		AssertArrays.assertEquals(3, ((IntegerValue) a.fetchValue(3)).asUInt16());

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
		AssertArrays.assertEquals(0, AssertArrays.makeUInt16ArrayEmpty().count());
		AssertArrays.assertEquals(1, UInt16Array.make(new char[] { 0 }).count());
		AssertArrays.assertEquals(2, UInt16Array.make(new char[] { 0, 1 }).count());
	}

	public void testStoreUInt16() {
		UInt16Array empty = UInt16Array.make(0);
		UInt16Array tri = UInt16Array.make(3);

		tri.storeUInt16(0, Character.MIN_VALUE);
		assertEquals(tri.uInt16At(0), Character.MIN_VALUE);
		tri.storeUInt16(1, (char) 1);
		assertEquals(tri.uInt16At(1), 1);
		tri.storeUInt16(2, Character.MAX_VALUE);
		assertEquals(tri.uInt16At(2), Character.MAX_VALUE);

		try {
			tri.storeUInt16(-1, (char) 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeUInt16(3, (char) 1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeUInt16(0, (char) 1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		UInt16Array empty = UInt16Array.make(0);
		UInt16Array tri = UInt16Array.make(3);

		// Store integer values within spec
		tri.storeInteger(0, IntegerValue.make(Character.MIN_VALUE));
		assertTrue(tri.uInt16At(0) == Character.MIN_VALUE);
		tri.storeInteger(1, IntegerValue.make(1));
		assertEquals(tri.uInt16At(1), 1);
		tri.storeInteger(2, IntegerValue.make(Character.MAX_VALUE));
		assertTrue(tri.uInt16At(2) == Character.MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeInteger(0, IntegerValue.make((int) Character.MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeInteger(2, IntegerValue.make((int) Character.MAX_VALUE + 1));
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
		UInt16Array empty = UInt16Array.make(0);
		UInt16Array tri = UInt16Array.make(3);

		// Store integer values within spec
		tri.storeValue(0, IntegerValue.make(Character.MIN_VALUE));
		assertTrue(tri.uInt16At(0) == Character.MIN_VALUE);
		tri.storeValue(1, IntegerValue.make(1));
		assertEquals(tri.uInt16At(1), 1);
		tri.storeValue(2, IntegerValue.make(Character.MAX_VALUE));
		assertTrue(tri.uInt16At(2) == Character.MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeValue(0, IntegerValue.make((int) Character.MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeValue(2, IntegerValue.make((int) Character.MAX_VALUE + 1));
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
		UInt16Array array = UInt16Array.make(0);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(UInt16Array.make(0), array);

		array = UInt16Array.make(1);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1 }), array);

		array = UInt16Array.make(3);
		array.storeAll(IntegerValue.make(2));
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 2, 2, 2 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(UInt16Array.make(new char[] { 1, 9, 9, 4, 5 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 0, 0, 4, 5 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeAll(IntegerValue.make(9), -1, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 9, 9, 9, 9 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeAll(IntegerValue.make(9), 2);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 9, 9, 3, 4, 5 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeAll(IntegerValue.make(9), 0, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2, 3, 4, 5 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeUInt16Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.storeAll(IntegerValue.make((int) Character.MAX_VALUE + 1), 1);
			fail();
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		UInt16Array array = AssertArrays.makeUInt16ArrayEmpty();
		array.storeMany(0, AssertArrays.makeUInt16ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeUInt16ArrayEmpty(), array);

		// simple
		array = AssertArrays.makeUInt16Array12345();
		array.storeMany(0, AssertArrays.makeUInt16Array12321());
		AssertArrays.assertEquals(AssertArrays.makeUInt16Array12321(), array);

		array = AssertArrays.makeUInt16Array12345();
		array.storeMany(0, AssertArrays.makeUInt16Array12321());
		AssertArrays.assertEquals(AssertArrays.makeUInt16Array12321(), array);

		array = AssertArrays.makeUInt16Array12321();
		array.storeMany(1, UInt16Array.make(new char[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 5, 4, 2, 1 }), array);

		array = AssertArrays.makeUInt16Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7 }));
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = AssertArrays.makeUInt16Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.storeMany(1, AssertArrays.makeUInt16Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.storeMany(0, AssertArrays.makeUInt16Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeUInt16Array12321();
		try {
			array.storeMany(1, Int32Array.make(new int[] { Character.MAX_VALUE + 1 }));
			fail("255");
		} catch (IllegalArgumentException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		UInt16Array array = AssertArrays.makeUInt16Array12345();
		char[] out = new char[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new char[] { 2, 3, 4 }));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new char[] { 1 }));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new char[] { 5 }));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new char[] { 1, 2, 3 }));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new char[] { 3, 4, 5 }));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new char[] { 4, 5, 0 }));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new char[] {
		}));

		array = AssertArrays.makeUInt16Array12345();
		out = new char[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeUInt16Array12345();
		out = new char[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		char[] charsE = new char[] {};
		char[] chars1 = new char[] { 1 };
		char[] chars12 = new char[] { 1, 2 };
		char[] chars11 = new char[] { 1, 1 };

		// Equal matches
		assertTrue(UInt16Array.make(charsE).isEqual(UInt16Array.make(charsE)));
		assertTrue(UInt16Array.make(chars1).isEqual(UInt16Array.make(chars1)));
		assertTrue(UInt16Array.make(chars12).isEqual(UInt16Array.make(chars12)));
		assertTrue(UInt16Array.make(chars1).isEqual(AssertArrays.makeInt32Array1()));
		
		// Unequal compatible matches
		assertFalse(UInt16Array.make(chars11).isEqual(UInt16Array.make(chars12)));
		assertFalse(UInt16Array.make(charsE).isEqual(UInt16Array.make(chars12)));
		assertFalse(UInt16Array.make(chars12).isEqual(UInt16Array.make(charsE)));

		// single value
		assertFalse(UInt16Array.make(chars1).isEqual(IntegerValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(UInt16Array.make(chars1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeUInt16ArrayEmpty().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array1().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt16Array1().indexOf(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12345().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt16Array12345().indexOf(IntegerValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12345().indexOf(IntegerValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12345().indexOf(IntegerValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeUInt16Array12321().indexOf(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeUInt16Array12321().indexOf(IEEE64Value.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = AssertArrays.makeUInt16ArrayEmpty().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array1().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array1().indexPast(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12345().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt16Array12345().indexPast(IntegerValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt16Array12345().indexPast(IntegerValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeUInt16Array12345().indexPast(IntegerValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt16Array12345().indexPast(IntegerValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt16Array12345().indexPast(IntegerValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeUInt16Array12321().indexPast(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeUInt16Array12321().indexPast(IEEE64Value.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		UInt16Array array = AssertArrays.makeUInt16ArrayEmpty();
		UInt16Array search = AssertArrays.makeUInt16Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeUInt16ArrayEmpty();
		search = AssertArrays.makeUInt16ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeUInt16Array12345();
		//		search = makeUInt16ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = UInt16Array.make(new char[] { 1, 2, 3, 1, 2 });
		search = UInt16Array.make(new char[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeUInt16Array12321();
		search = UInt16Array.make(new char[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = UInt16Array.make(new char[] { 1, 2, 3, 1, 2 });
		Int32Array searchInt32 = Int32Array.make(new int[] { 1, 2 });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = UInt16Array.make(new char[] { 1, 2, 3, 1, 2 });
		search = UInt16Array.make(new char[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = UInt16Array.make(new char[] { 1, 1, 1 });
		search = UInt16Array.make(new char[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeUInt16Array12321();
		search = UInt16Array.make(new char[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeUInt16Array12321();
		search = UInt16Array.make(new char[] { 1, 2 });
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
		array = AssertArrays.makeUInt16Array12321();
		search = UInt16Array.make(new char[] { 1, 2 });
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
		array = UInt16Array.make(new char[] { 1, 2, 3, 1, 2 });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		UInt16Array array = AssertArrays.makeUInt16ArrayEmpty();
		array.addElements(0, AssertArrays.makeUInt16Array12321(), -1, 0);
		assertEquals(AssertArrays.makeUInt16ArrayEmpty(), array);

		array = AssertArrays.makeUInt16Array1();
		array.addElements(0, UInt16Array.make(new char[] { 9 }), -1, 0);
		assertEquals(UInt16Array.make(new char[] { 10 }), array);

		array = UInt16Array.make(5);
		array.addElements(0, AssertArrays.makeUInt16Array12321(), -1, 0);
		assertEquals(AssertArrays.makeUInt16Array12321(), array);

		array = AssertArrays.makeUInt16Array12345();
		array.addElements(0, AssertArrays.makeUInt16Array12321(), -1, 0);
		assertEquals(UInt16Array.make(new char[] { 2, 4, 6, 6, 6 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.addElements(2, AssertArrays.makeUInt16Array12321(), -1, 0);
		assertEquals(UInt16Array.make(new char[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.addElements(2, AssertArrays.makeUInt16Array12321(), -1);
		assertEquals(UInt16Array.make(new char[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.addElements(2, AssertArrays.makeUInt16Array12321());
		assertEquals(UInt16Array.make(new char[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.addElements(2, AssertArrays.makeUInt16Array12321(), 2, 1);
		assertEquals(UInt16Array.make(new char[] { 1, 2, 5, 7, 5 }), array);

		// element arithmetic overflows
		array = UInt16Array.make(new char[] {40000, 0});
		array.addElements(0, UInt16Array.make(new char[] {40000, 40000}));
		assertEquals(UInt16Array.make(new char[]{14464, 40000}), array);

		array = AssertArrays.makeUInt16Array12345();
		try {
			array.addElements(2, AssertArrays.makeUInt16Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeUInt16ArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		UInt16Array array = AssertArrays.makeUInt16ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeUInt16Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeUInt16ArrayEmpty(), array);

		array = AssertArrays.makeUInt16Array1();
		array.subtractElements(0, UInt16Array.make(new char[] { 1 }), -1, 0);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 0 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.subtractElements(0, AssertArrays.makeUInt16Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 0, 0, 0, 2, 4 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.subtractElements(2, AssertArrays.makeUInt16Array12321(), -1, 0);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.subtractElements(2, AssertArrays.makeUInt16Array12321(), -1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.subtractElements(2, AssertArrays.makeUInt16Array12321());
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeUInt16Array12345();
		array.subtractElements(2, AssertArrays.makeUInt16Array12321(), 2, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2, 1, 1, 5 }), array);

		// element arithmetic overflows
		array = UInt16Array.make(new char[] {0, 40000});
		array.subtractElements(0, UInt16Array.make(new char[] {40000, 40000}));
		assertEquals(UInt16Array.make(new char[]{25536, 0}), array);

		// extend count beyond end of array
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeUInt16Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeUInt16ArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		UInt16Array array1 = AssertArrays.makeUInt16ArrayEmpty();
		UInt16Array array2 = AssertArrays.makeUInt16ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeUInt16Array12345();
		array2 = AssertArrays.makeUInt16Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeUInt16Array12321();
		array2 = AssertArrays.makeUInt16Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeUInt16Array12321();
		array2 = AssertArrays.makeUInt16Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeUInt16Array12345();
		array2 = AssertArrays.makeUInt16Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeUInt16ArrayEmpty();
		array2 = AssertArrays.makeUInt16Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeUInt16Array1();
		array2 = AssertArrays.makeUInt16ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = UInt16Array.make(new char[] { 0, 0 });
		array2 = UInt16Array.make(new char[] { 0 });
		assertEquals(0, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeUInt16Array12321();
		array2 = AssertArrays.makeUInt16Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeUInt16Array12321();
		array2 = AssertArrays.makeUInt16Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeUInt16Array12321();
		array2 = AssertArrays.makeUInt16Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeUInt16Array12321();
		array2 = AssertArrays.makeUInt16Array12345();
		assertEquals(-1, array1.compare(array2, 10));

		// different array types
		array1 = AssertArrays.makeUInt16Array12345();
		Int32Array array2Int32 = AssertArrays.makeInt32Array12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeUInt16Array12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		UInt16Array array = AssertArrays.makeUInt16ArrayEmpty();
		UInt16Array copy = (UInt16Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeUInt16ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeUInt16Array12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copyGrow(3);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		UInt16Array array = AssertArrays.makeUInt16ArrayEmpty();
		UInt16Array copy = (UInt16Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeUInt16ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeUInt16Array12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeUInt16Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 0);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 1, 2 }), copy);

		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 3);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 4, 5 }), copy);

		// partial copy with too large count
		array = AssertArrays.makeUInt16Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeUInt16Array12345();
		copy = (UInt16Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(UInt16Array.make(new char[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeUInt16ArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", AssertArrays.makeUInt16Array12345().toString());
	}
	
	public void testString() {
		UInt16Array array = UInt16Array.make("");
		assertEquals(0, array.count());
		
		array = UInt16Array.make("a \u0424\t");
		assertEquals(4, array.count());
		assertEquals('a', array.uInt16At(0));
		assertEquals(' ', array.uInt16At(1));
		assertEquals('\u0424', array.uInt16At(2));
		assertEquals('\t', array.uInt16At(3));
	}
	
	public void testAsString() {
		UInt16Array array = UInt16Array.make("");
		assertEquals("", array.asString());
		
		array = UInt16Array.make("a \u0424\t");
		assertEquals("a \u0424\t", array.asString());
	}

}
