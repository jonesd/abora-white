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

import org.abora.white.collection.arrays.IEEE64Array;
import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.Int8Array;
import org.abora.white.collection.arrays.PrimIntegerArray;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IEEE64Value;
import org.abora.white.value.IntegerValue;

public class Int8ArrayTest extends TestCase {

	public Int8ArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(Int8ArrayTest.class);
	}

	protected Int8Array makeInt8ArrayEmpty() {
		return Int8Array.make(new byte[] {
		});
	}
	protected Int8Array makeInt8Array1() {
		return Int8Array.make(new byte[] { 1 });
	}
	protected Int8Array makeInt8Array12345() {
		return Int8Array.make(new byte[] { 1, 2, 3, 4, 5 });
	}
	protected Int8Array makeInt8Array12321() {
		return Int8Array.make(new byte[] { 1, 2, 3, 2, 1 });
	}

	public void assertEquals(PrimIntegerArray expected, PrimIntegerArray actual) {
		assertEquals(expected.count(), actual.count());
		for (int i = 0; i < expected.count(); i++) {
			IntegerValue expectedValue = expected.integerAt(i);
			IntegerValue actualValue = actual.integerAt(i);
			assertEquals(expectedValue, actualValue);
		}
	}

	public void testMakeCount() {
		Int8Array array = Int8Array.make(0);
		assertEquals(0, array.count());

		array = Int8Array.make(1);
		assertEquals(1, array.count());
		assertEquals(0, array.int8At(0));
	}

	public void testInt8At() {
		Int8Array a = Int8Array.make(new byte[] { 0, 1, -2, 3 });

		assertEquals(0, a.int8At(0));
		assertEquals(1, a.int8At(1));
		assertEquals(-2, a.int8At(2));
		assertEquals(3, a.int8At(3));

		try {
			a.int8At(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.int8At(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testInt8AtEmpty() {
		Int8Array a = Int8Array.make(0);
		try {
			a.int8At(0);
			fail("expected 0 failure");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		Int8Array a = Int8Array.make(new byte[] { 0, 1, -2, 3 });

		assertEquals(IntegerValue.make(0), a.integerAt(0));
		assertEquals(IntegerValue.make(1), a.integerAt(1));
		assertEquals(IntegerValue.make(-2), a.integerAt(2));
		assertEquals(IntegerValue.make(3), a.integerAt(3));

		try {
			a.integerAt(-1);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.integerAt(4);
			fail("expected -1 failure");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testFetchValue() {
		Int8Array a = Int8Array.make(new byte[] { 0, 1, -2, 3 });

		assertEquals(0, ((IntegerValue) a.fetchValue(0)).asInt8());
		assertEquals(1, ((IntegerValue) a.fetchValue(1)).asInt8());
		assertEquals(-2, ((IntegerValue) a.fetchValue(2)).asInt8());
		assertEquals(3, ((IntegerValue) a.fetchValue(3)).asInt8());

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
		assertEquals(0, Int8Array.make(new byte[] {
		}).count());
		assertEquals(1, Int8Array.make(new byte[] { 0 }).count());
		assertEquals(2, Int8Array.make(new byte[] { 0, 1 }).count());
	}

	public void testStoreInt8() {
		Int8Array empty = Int8Array.make(0);
		Int8Array tri = Int8Array.make(3);

		tri.storeInt8(0, Byte.MIN_VALUE);
		assertEquals(tri.int8At(0), Byte.MIN_VALUE);
		tri.storeInt8(1, (byte) 1);
		assertEquals(tri.int8At(1), 1);
		tri.storeInt8(2, Byte.MAX_VALUE);
		assertEquals(tri.int8At(2), Byte.MAX_VALUE);

		try {
			tri.storeInt8(-1, (byte) 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeInt8(3, (byte) 1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeInt8(0, (byte) 1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		Int8Array empty = Int8Array.make(0);
		Int8Array tri = Int8Array.make(3);

		// Store integer values within spec
		tri.storeInteger(0, IntegerValue.make(Byte.MIN_VALUE));
		assertTrue(tri.int8At(0) == Byte.MIN_VALUE);
		tri.storeInteger(1, IntegerValue.make(1));
		assertEquals(tri.int8At(1), 1);
		tri.storeInteger(2, IntegerValue.make(Byte.MAX_VALUE));
		assertTrue(tri.int8At(2) == Byte.MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeInteger(0, IntegerValue.make((int) Byte.MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeInteger(2, IntegerValue.make((int) Byte.MAX_VALUE + 1));
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
		Int8Array empty = Int8Array.make(0);
		Int8Array tri = Int8Array.make(3);

		// Store integer values within spec
		tri.storeValue(0, IntegerValue.make(Byte.MIN_VALUE));
		assertTrue(tri.int8At(0) == Byte.MIN_VALUE);
		tri.storeValue(1, IntegerValue.make(1));
		assertEquals(tri.int8At(1), 1);
		tri.storeValue(2, IntegerValue.make(Byte.MAX_VALUE));
		assertTrue(tri.int8At(2) == Byte.MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeValue(0, IntegerValue.make((int) Byte.MIN_VALUE - 1));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeValue(2, IntegerValue.make((int) Byte.MAX_VALUE + 1));
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
		//TODO store with classcast
	}

	public void testStoreAll() {
		Int8Array array = Int8Array.make(0);
		array.storeAll(IntegerValue.make(1));
		assertEquals(Int8Array.make(0), array);

		array = Int8Array.make(1);
		array.storeAll(IntegerValue.make(1));
		assertEquals(Int8Array.make(new byte[] { 1 }), array);

		array = Int8Array.make(3);
		array.storeAll(IntegerValue.make(2));
		assertEquals(Int8Array.make(new byte[] { 2, 2, 2 }), array);

		array = makeInt8Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 9, 9, 4, 5 }), array);

		array = makeInt8Array12345();
		array.storeAll(null, 2, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 0, 0, 4, 5 }), array);

		array = makeInt8Array12345();
		array.storeAll(IntegerValue.make(9), -1, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 9, 9, 9, 9 }), array);

		array = makeInt8Array12345();
		array.storeAll(IntegerValue.make(9), 2);
		assertEquals(Int8Array.make(new byte[] { 9, 9, 3, 4, 5 }), array);

		array = makeInt8Array12345();
		array.storeAll(IntegerValue.make(9), 0, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 3, 4, 5 }), array);

		array = makeInt8Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = makeInt8Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = makeInt8Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = makeInt8Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		Int8Array array = makeInt8ArrayEmpty();
		array.storeMany(0, makeInt8ArrayEmpty());
		assertEquals(makeInt8ArrayEmpty(), array);

		// simple
		array = makeInt8Array12345();
		array.storeMany(0, makeInt8Array12321());
		assertEquals(makeInt8Array12321(), array);

		array = makeInt8Array12345();
		array.storeMany(0, makeInt8Array12321());
		assertEquals(makeInt8Array12321(), array);

		array = makeInt8Array12321();
		array.storeMany(1, Int8Array.make(new byte[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		assertEquals(Int8Array.make(new byte[] { 1, 5, 4, 2, 1 }), array);

		array = makeInt8Array12321();
		array.storeMany(1, Int32Array.make(new int[] { 8, 7 }));
		assertEquals(Int8Array.make(new byte[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = makeInt8Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = makeInt8Array12345();
		try {
			array.storeMany(1, makeInt8Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = makeInt8Array12345();
		try {
			array.storeMany(0, makeInt8Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		Int8Array array = makeInt8Array12345();
		byte[] out = new byte[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new byte[] { 2, 3, 4 }));

		array = makeInt8Array12345();
		out = new byte[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new byte[] { 1 }));

		array = makeInt8Array12345();
		out = new byte[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new byte[] { 5 }));

		array = makeInt8Array12345();
		out = new byte[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new byte[] { 1, 2, 3 }));

		array = makeInt8Array12345();
		out = new byte[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new byte[] { 3, 4, 5 }));

		array = makeInt8Array12345();
		out = new byte[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new byte[] { 4, 5, 0 }));

		array = makeInt8Array12345();
		out = new byte[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new byte[] {
		}));

		array = makeInt8Array12345();
		out = new byte[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = makeInt8Array12345();
		out = new byte[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		byte[] bytesE = new byte[] {};
		byte[] bytes1 = new byte[] { 1 };
		byte[] bytes12 = new byte[] { 1, 2 };
		byte[] bytes11 = new byte[] { 1, 1 };

		assertTrue(Int8Array.make(bytesE).isEqual(Int8Array.make(bytesE)));
		assertTrue(Int8Array.make(bytes1).isEqual(Int8Array.make(bytes1)));
		assertTrue(Int8Array.make(bytes12).isEqual(Int8Array.make(bytes12)));
		
		assertFalse(Int8Array.make(bytes11).isEqual(Int8Array.make(bytes12)));
		assertFalse(Int8Array.make(bytesE).isEqual(Int8Array.make(bytes12)));
		assertFalse(Int8Array.make(bytes1).isEqual(IntegerValue.make(1)));
		assertFalse(Int8Array.make(bytes12).isEqual(Int8Array.make(bytesE)));
	}

	public void testIndexOf() {
		int index = makeInt8ArrayEmpty().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = makeInt8Array1().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = makeInt8Array1().indexOf(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = makeInt8Array12345().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = makeInt8Array12345().indexOf(IntegerValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = makeInt8Array12345().indexOf(IntegerValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = makeInt8Array12345().indexOf(IntegerValue.make(5), 0, 1);
		assertEquals(4, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), 0, 1);
		assertEquals(1, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), 0, 2);
		assertEquals(3, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(1), -1, -1);
		assertEquals(4, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), -1, -1);
		assertEquals(3, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), -1, -2);
		assertEquals(1, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), -3, -1);
		assertEquals(1, index);

		index = makeInt8Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			makeInt8Array12321().indexOf(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			makeInt8Array12321().indexOf(IEEE64Value.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = makeInt8ArrayEmpty().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = makeInt8Array1().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = makeInt8Array1().indexPast(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = makeInt8Array12345().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(1, index);

		index = makeInt8Array12345().indexPast(IntegerValue.make(1), 0, 2);
		assertEquals(2, index);

		index = makeInt8Array12345().indexPast(IntegerValue.make(1), 1, 1);
		assertEquals(1, index);

		index = makeInt8Array12345().indexPast(IntegerValue.make(5), 0, 1);
		assertEquals(0, index);

		index = makeInt8Array12345().indexPast(IntegerValue.make(5), 3, 1);
		assertEquals(3, index);

		index = makeInt8Array12345().indexPast(IntegerValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), 0, 1);
		assertEquals(0, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), 0, 2);
		assertEquals(2, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(1), -1, -1);
		assertEquals(3, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), -1, -1);
		assertEquals(4, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), -1, 1);
		assertEquals(4, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), -1, -2);
		assertEquals(2, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), -1, -3);
		assertEquals(0, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(2), -3, -1);
		assertEquals(2, index);

		index = makeInt8Array12321().indexPast(IntegerValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			makeInt8Array12321().indexPast(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			makeInt8Array12321().indexPast(IEEE64Value.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		Int8Array array = makeInt8ArrayEmpty();
		Int8Array search = makeInt8Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = makeInt8ArrayEmpty();
		search = makeInt8ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeInt8Array12345();
		//		search = makeInt8ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = Int8Array.make(new byte[] { 1, 2, 3, 1, 2 });
		search = Int8Array.make(new byte[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = makeInt8Array12321();
		search = Int8Array.make(new byte[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// reverse search		
		array = Int8Array.make(new byte[] { 1, 2, 3, 1, 2 });
		search = Int8Array.make(new byte[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = Int8Array.make(new byte[] { 1, 1, 1 });
		search = Int8Array.make(new byte[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = makeInt8Array12321();
		search = Int8Array.make(new byte[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = makeInt8Array12321();
		search = Int8Array.make(new byte[] { 1, 2 });
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
		array = makeInt8Array12321();
		search = Int8Array.make(new byte[] { 1, 2 });
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
		Int8Array array = makeInt8ArrayEmpty();
		array.addElements(0, makeInt8Array12321(), -1, 0);
		assertEquals(makeInt8ArrayEmpty(), array);

		array = makeInt8Array1();
		array.addElements(0, Int8Array.make(new byte[] { 9 }), -1, 0);
		assertEquals(Int8Array.make(new byte[] { 10 }), array);

		array = Int8Array.make(5);
		array.addElements(0, makeInt8Array12321(), -1, 0);
		assertEquals(makeInt8Array12321(), array);

		array = makeInt8Array12345();
		array.addElements(0, makeInt8Array12321(), -1, 0);
		assertEquals(Int8Array.make(new byte[] { 2, 4, 6, 6, 6 }), array);

		array = makeInt8Array12345();
		array.addElements(2, makeInt8Array12321(), -1, 0);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 4, 6, 8 }), array);

		array = makeInt8Array12345();
		array.addElements(2, makeInt8Array12321(), -1);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 4, 6, 8 }), array);

		array = makeInt8Array12345();
		array.addElements(2, makeInt8Array12321());
		assertEquals(Int8Array.make(new byte[] { 1, 2, 4, 6, 8 }), array);

		array = makeInt8Array12345();
		array.addElements(2, makeInt8Array12321(), 2, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 5, 7, 5 }), array);

		array = makeInt8Array12345();
		try {
			array.addElements(2, makeInt8Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		//TODO class cast
		//TODO adding beyound size of values
	}

	public void testSubtractElements() {
		Int8Array array = makeInt8ArrayEmpty();
		array.subtractElements(0, makeInt8Array12321(), -1, 0);
		assertEquals(makeInt8ArrayEmpty(), array);

		array = makeInt8Array1();
		array.subtractElements(0, Int8Array.make(new byte[] { 9 }), -1, 0);
		assertEquals(Int8Array.make(new byte[] { -8 }), array);

		array = Int8Array.make(5);
		array.subtractElements(0, makeInt8Array12321(), -1, 0);
		assertEquals(Int8Array.make(new byte[] { -1, -2, -3, -2, -1 }), array);

		array = makeInt8Array12345();
		array.subtractElements(0, makeInt8Array12321(), -1, 0);
		assertEquals(Int8Array.make(new byte[] { 0, 0, 0, 2, 4 }), array);

		array = makeInt8Array12345();
		array.subtractElements(2, makeInt8Array12321(), -1, 0);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 2, 2, 2 }), array);

		array = makeInt8Array12345();
		array.subtractElements(2, makeInt8Array12321(), -1);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 2, 2, 2 }), array);

		array = makeInt8Array12345();
		array.subtractElements(2, makeInt8Array12321());
		assertEquals(Int8Array.make(new byte[] { 1, 2, 2, 2, 2 }), array);

		array = makeInt8Array12345();
		array.subtractElements(2, makeInt8Array12321(), 2, 1);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 1, 1, 5 }), array);

		array = makeInt8Array12345();
		try {
			array.subtractElements(2, makeInt8Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		Int8Array array1 = makeInt8ArrayEmpty();
		Int8Array array2 = makeInt8ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = makeInt8Array12345();
		array2 = makeInt8Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = makeInt8Array12321();
		array2 = makeInt8Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = makeInt8Array12321();
		array2 = makeInt8Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = makeInt8Array12345();
		array2 = makeInt8Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = makeInt8ArrayEmpty();
		array2 = makeInt8Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = makeInt8Array1();
		array2 = makeInt8ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = Int8Array.make(new byte[] { 0, 0 });
		array2 = Int8Array.make(new byte[] { 0 });
		assertEquals(0, array1.compare(array2));

		// compare sub-regions		
		array1 = makeInt8Array12321();
		array2 = makeInt8Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = makeInt8Array12321();
		array2 = makeInt8Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = makeInt8Array12321();
		array2 = makeInt8Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = makeInt8Array12321();
		array2 = makeInt8Array12345();
		assertEquals(-1, array1.compare(array2, 10));
	}

	public void testCopyGrow() {
		Int8Array array = makeInt8ArrayEmpty();
		Int8Array copy = (Int8Array) array.copyGrow(0);
		assertEquals(makeInt8ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = makeInt8Array12345();
		copy = (Int8Array) array.copyGrow(0);
		assertEquals(makeInt8Array12345(), copy);
		assertNotSame(array, copy);

		array = makeInt8Array12345();
		copy = (Int8Array) array.copyGrow(3);
		assertEquals(Int8Array.make(new byte[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		Int8Array array = makeInt8ArrayEmpty();
		Int8Array copy = (Int8Array) array.copy();
		assertEquals(makeInt8ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = makeInt8Array12345();
		copy = (Int8Array) array.copy();
		assertEquals(makeInt8Array12345(), copy);
		assertEquals(makeInt8Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 1);
		assertEquals(Int8Array.make(new byte[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 0);
		assertEquals(Int8Array.make(new byte[] { 1, 2 }), copy);

		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 3);
		assertEquals(Int8Array.make(new byte[] { 4, 5 }), copy);

		// partial copy with too large count
		array = makeInt8Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 1);
		assertEquals(Int8Array.make(new byte[] { 2, 3 }), copy);

		// partial copy with leading space
		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 1, 1);
		assertEquals(Int8Array.make(new byte[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 1, 0, 1);
		assertEquals(Int8Array.make(new byte[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = makeInt8Array12345();
		copy = (Int8Array) array.copy(2, 1, 2, 1);
		assertEquals(Int8Array.make(new byte[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", makeInt8ArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", makeInt8Array12345().toString());
	}
}
