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
import org.abora.white.collection.arrays.IntegerVarArray;
import org.abora.white.collection.arrays.Int64Array;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IEEE64Value;
import org.abora.white.value.IntegerValue;

public class IntegerVarArrayTest extends TestCase {

	public IntegerVarArrayTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(IntegerVarArrayTest.class);
	}

	public void testMakeCount() {
		IntegerVarArray array = IntegerVarArray.make(0);
		assertEquals(0, array.count());

		array = IntegerVarArray.make(1);
		AssertArrays.assertEquals(1, array.count());
		AssertArrays.assertEquals(IntegerValue.zero(), array.integerVarAt(0));
		
		try {
			IntegerVarArray.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testIntegerVarAt() {
		IntegerVarArray a = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(1), IntegerValue.make(-2), IntegerValue.make(3) });

		AssertArrays.assertEquals(IntegerValue.zero(), a.integerVarAt(0));
		AssertArrays.assertEquals(IntegerValue.make(1), a.integerVarAt(1));
		AssertArrays.assertEquals(IntegerValue.make(-2), a.integerVarAt(2));
		AssertArrays.assertEquals(IntegerValue.make(3), a.integerVarAt(3));

		try {
			a.integerVarAt(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.integerVarAt(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testIntegerVarAtEmpty() {
		IntegerVarArray a = IntegerVarArray.make(0);
		try {
			a.integerVarAt(0);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		IntegerVarArray a = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(1), IntegerValue.make(-2), IntegerValue.make(3) });

		AssertArrays.assertEquals(IntegerValue.make(0), a.integerAt(0));
		AssertArrays.assertEquals(IntegerValue.make(1), a.integerAt(1));
		AssertArrays.assertEquals(IntegerValue.make(-2), a.integerAt(2));
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
		IntegerVarArray a = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(1), IntegerValue.make(-2), IntegerValue.make(3) });

		AssertArrays.assertEquals(0, ((IntegerValue) a.fetchValue(0)).asInt32());
		AssertArrays.assertEquals(1, ((IntegerValue) a.fetchValue(1)).asInt32());
		AssertArrays.assertEquals(-2, ((IntegerValue) a.fetchValue(2)).asInt32());
		AssertArrays.assertEquals(3, ((IntegerValue) a.fetchValue(3)).asInt32());

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
		AssertArrays.assertEquals(0, AssertArrays.makeIntegerVarArrayEmpty().count());
		AssertArrays.assertEquals(1, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0) }).count());
		AssertArrays.assertEquals(2, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(1) }).count());
	}

	public void testStoreIntegerVar() {
		IntegerVarArray empty = IntegerVarArray.make(0);
		IntegerVarArray tri = IntegerVarArray.make(3);

		tri.storeIntegerVar(0, IntegerValue.make(Integer.MIN_VALUE));
		assertEquals(IntegerValue.make(Integer.MIN_VALUE), tri.integerVarAt(0));
		tri.storeIntegerVar(1, IntegerValue.make(1));
		assertEquals(IntegerValue.make(1), tri.integerVarAt(1));
		tri.storeIntegerVar(2, IntegerValue.make(Integer.MAX_VALUE));
		assertEquals(IntegerValue.make(Integer.MAX_VALUE), tri.integerVarAt(2));

		try {
			tri.storeIntegerVar(-1, IntegerValue.make(1));
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeIntegerVar(3, IntegerValue.make(1));
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeIntegerVar(0, IntegerValue.make(1));
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		IntegerVarArray empty = IntegerVarArray.make(0);
		IntegerVarArray tri = IntegerVarArray.make(3);

		// Store integer values within spec
		tri.storeInteger(0, IntegerValue.make(Integer.MIN_VALUE));
		assertEquals(IntegerValue.make(Integer.MIN_VALUE), tri.integerVarAt(0));
		tri.storeInteger(1, IntegerValue.make(1));
		assertEquals(IntegerValue.make(1), tri.integerVarAt(1));
		tri.storeInteger(2, IntegerValue.make(Integer.MAX_VALUE));
		assertEquals(IntegerValue.make(Integer.MAX_VALUE), tri.integerVarAt(2));

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
		IntegerVarArray empty = IntegerVarArray.make(0);
		IntegerVarArray tri = IntegerVarArray.make(3);

		// Store integer values within spec
		tri.storeValue(0, IntegerValue.make(Integer.MIN_VALUE));
		assertEquals(IntegerValue.make(Integer.MIN_VALUE), tri.integerVarAt(0));
		tri.storeValue(1, IntegerValue.make(1));
		assertEquals(IntegerValue.make(1), tri.integerVarAt(1));
		tri.storeValue(2, IntegerValue.make(Integer.MAX_VALUE));
		assertEquals(IntegerValue.make(Integer.MAX_VALUE), tri.integerVarAt(2));

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
		// Null value
		try {tri.storeValue(0, null);
			fail("null");
		} catch (NullPointerException e) {
			// expected
		}
	}

	public void testStoreAll() {
		IntegerVarArray array = IntegerVarArray.make(0);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(IntegerVarArray.make(0), array);

		array = IntegerVarArray.make(1);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1) }), array);

		array = IntegerVarArray.make(3);
		array.storeAll(IntegerValue.make(2));
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(9), IntegerValue.make(9), IntegerValue.make(4), IntegerValue.make(5) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(4), IntegerValue.make(5) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeAll(IntegerValue.make(9), -1, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(9), IntegerValue.make(9), IntegerValue.make(9), IntegerValue.make(9) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeAll(IntegerValue.make(9), 2);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(9), IntegerValue.make(9), IntegerValue.make(3), IntegerValue.make(4), IntegerValue.make(5) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeAll(IntegerValue.make(9), 0, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(4), IntegerValue.make(5) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(9), IntegerValue.make(9), IntegerValue.make(4), IntegerValue.make(5) }), array);

		// Store outside of array bounds
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.storeAll(IntegerValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.storeAll(IntegerValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		IntegerVarArray array = AssertArrays.makeIntegerVarArrayEmpty();
		array.storeMany(0, AssertArrays.makeIntegerVarArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArrayEmpty(), array);

		// simple
		array = AssertArrays.makeIntegerVarArray12345();
		array.storeMany(0, AssertArrays.makeIntegerVarArray12321());
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArray12321(), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.storeMany(0, AssertArrays.makeIntegerVarArray12321());
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArray12321(), array);

		array = AssertArrays.makeIntegerVarArray12321();
		array.storeMany(1, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(8), IntegerValue.make(7), IntegerValue.make(6), IntegerValue.make(5), IntegerValue.make(4), IntegerValue.make(3), IntegerValue.make(3) }), 2, 3);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(5), IntegerValue.make(4), IntegerValue.make(2), IntegerValue.make(1) }), array);

		array = AssertArrays.makeIntegerVarArray12321();
		array.storeMany(1, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(8), IntegerValue.make(7) }));
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(8), IntegerValue.make(7), IntegerValue.make(2), IntegerValue.make(1) }), array);

		// Store incompatible type
		array = AssertArrays.makeIntegerVarArray12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.storeMany(1, AssertArrays.makeIntegerVarArray12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.storeMany(0, AssertArrays.makeIntegerVarArray12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		IntegerVarArray array = AssertArrays.makeIntegerVarArray12345();
		IntegerValue[] out = new IntegerValue[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new IntegerValue[] { IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(4) }));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new IntegerValue[] { IntegerValue.make(1) }));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new IntegerValue[] { IntegerValue.make(5) }));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3) }));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new IntegerValue[] { IntegerValue.make(3), IntegerValue.make(4), IntegerValue.make(5) }));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[3];
		array.copyToBuffer(out, -1, 3);
		//TODO should the following null actually be a 0?
		assertTrue(Arrays.equals(out, new IntegerValue[] { IntegerValue.make(4), IntegerValue.make(5), null }));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new IntegerValue[] {
		}));

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeIntegerVarArray12345();
		out = new IntegerValue[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		IntegerValue[] intsE = new IntegerValue[] {};
		IntegerValue[] ints1 = new IntegerValue[] { IntegerValue.make(1) };
		IntegerValue[] ints12 = new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) };
		IntegerValue[] ints11 = new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(1) };

		// Equal matches
		assertTrue(IntegerVarArray.make(intsE).isEqual(IntegerVarArray.make(intsE)));
		assertTrue(IntegerVarArray.make(ints1).isEqual(IntegerVarArray.make(ints1)));
		assertTrue(IntegerVarArray.make(ints12).isEqual(IntegerVarArray.make(ints12)));
		assertTrue(IntegerVarArray.make(ints1).isEqual(AssertArrays.makeIntegerVarArray1()));
		
		// Unequal compatible matches
		assertFalse(IntegerVarArray.make(ints11).isEqual(IntegerVarArray.make(ints12)));
		assertFalse(IntegerVarArray.make(intsE).isEqual(IntegerVarArray.make(ints12)));
		assertFalse(IntegerVarArray.make(ints12).isEqual(IntegerVarArray.make(intsE)));

		// single value
		assertFalse(IntegerVarArray.make(ints1).isEqual(IntegerValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(IntegerVarArray.make(ints1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeIntegerVarArrayEmpty().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray1().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIntegerVarArray1().indexOf(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12345().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIntegerVarArray12345().indexOf(IntegerValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12345().indexOf(IntegerValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12345().indexOf(IntegerValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeIntegerVarArray12321().indexOf(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeIntegerVarArray12321().indexOf(IEEE64Value.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = AssertArrays.makeIntegerVarArrayEmpty().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray1().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray1().indexPast(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12345().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIntegerVarArray12345().indexPast(IntegerValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeIntegerVarArray12345().indexPast(IntegerValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeIntegerVarArray12345().indexPast(IntegerValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIntegerVarArray12345().indexPast(IntegerValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeIntegerVarArray12345().indexPast(IntegerValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeIntegerVarArray12321().indexPast(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeIntegerVarArray12321().indexPast(IEEE64Value.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		IntegerVarArray array = AssertArrays.makeIntegerVarArrayEmpty();
		IntegerVarArray search = AssertArrays.makeIntegerVarArray1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeIntegerVarArrayEmpty();
		search = AssertArrays.makeIntegerVarArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeIntegerVarArray12345();
		//		search = makeIntegerVarArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(1), IntegerValue.make(2) });
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeIntegerVarArray12321();
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(4), IntegerValue.make(9), IntegerValue.make(2), IntegerValue.make(8) });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(1), IntegerValue.make(2) });
		IntegerVarArray searchInt32 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(1), IntegerValue.make(2) });
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(1), IntegerValue.make(1) });
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(1) });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeIntegerVarArray12321();
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1) });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeIntegerVarArray12321();
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) });
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
		array = AssertArrays.makeIntegerVarArray12321();
		search = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) });
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
		array = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(1), IntegerValue.make(2) });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		IntegerVarArray array = AssertArrays.makeIntegerVarArrayEmpty();
		array.addElements(0, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		assertEquals(AssertArrays.makeIntegerVarArrayEmpty(), array);

		array = AssertArrays.makeIntegerVarArray1();
		array.addElements(0, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(9) }), -1, 0);
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(10) }), array);

		array = IntegerVarArray.make(5);
		array.addElements(0, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		assertEquals(AssertArrays.makeIntegerVarArray12321(), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.addElements(0, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(2), IntegerValue.make(4), IntegerValue.make(6), IntegerValue.make(6), IntegerValue.make(6) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.addElements(2, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(4), IntegerValue.make(6), IntegerValue.make(8) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.addElements(2, AssertArrays.makeIntegerVarArray12321(), -1);
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(4), IntegerValue.make(6), IntegerValue.make(8) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.addElements(2, AssertArrays.makeIntegerVarArray12321());
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(4), IntegerValue.make(6), IntegerValue.make(8) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.addElements(2, AssertArrays.makeIntegerVarArray12321(), 2, 1);
		assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(5), IntegerValue.make(7), IntegerValue.make(5) }), array);

		// compatible array types
		array = AssertArrays.makeIntegerVarArray12345();
		array.addElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[]{ IntegerValue.make(2), IntegerValue.make(4), IntegerValue.make(6), IntegerValue.make(6), IntegerValue.make(6)}), array);

		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.addElements(2, AssertArrays.makeIntegerVarArray12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeIntegerVarArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		IntegerVarArray array = AssertArrays.makeIntegerVarArrayEmpty();
		array.subtractElements(0, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArrayEmpty(), array);

		array = AssertArrays.makeIntegerVarArray1();
		array.subtractElements(0, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(9) }), -1, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(-8) }), array);

		array = IntegerVarArray.make(5);
		array.subtractElements(0, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(-1), IntegerValue.make(-2), IntegerValue.make(-3), IntegerValue.make(-2), IntegerValue.make(-1) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.subtractElements(0, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(2), IntegerValue.make(4) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.subtractElements(2, AssertArrays.makeIntegerVarArray12321(), -1, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.subtractElements(2, AssertArrays.makeIntegerVarArray12321(), -1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.subtractElements(2, AssertArrays.makeIntegerVarArray12321());
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2), IntegerValue.make(2) }), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.subtractElements(2, AssertArrays.makeIntegerVarArray12321(), 2, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(1), IntegerValue.make(1), IntegerValue.make(5) }), array);

		// compatible array types
		array = AssertArrays.makeIntegerVarArray12345();
		array.subtractElements(0, AssertArrays.makeInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[]{IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(2), IntegerValue.make(4)}), array);

		// extend count beyond end of array
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.subtractElements(2, AssertArrays.makeIntegerVarArray12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeIntegerVarArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		IntegerVarArray array1 = AssertArrays.makeIntegerVarArrayEmpty();
		IntegerVarArray array2 = AssertArrays.makeIntegerVarArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeIntegerVarArray12345();
		array2 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeIntegerVarArray12321();
		array2 = AssertArrays.makeIntegerVarArray12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeIntegerVarArray12321();
		array2 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeIntegerVarArray12345();
		array2 = AssertArrays.makeIntegerVarArray12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeIntegerVarArrayEmpty();
		array2 = AssertArrays.makeIntegerVarArray1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeIntegerVarArray1();
		array2 = AssertArrays.makeIntegerVarArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(0) });
		array2 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0) });
		assertEquals(0, array1.compare(array2));

		array1 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(-1) });
		array2 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1) });
		assertEquals(-1, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeIntegerVarArray12321();
		array2 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeIntegerVarArray12321();
		array2 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeIntegerVarArray12321();
		array2 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeIntegerVarArray12321();
		array2 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(-1, array1.compare(array2, 10));

		// compare near minimum held value
		array1 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(Integer.MIN_VALUE)});
		array2 = IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(Integer.MAX_VALUE)});
		assertEquals(-1, array1.compare(array2));

		// different array types
		array1 = AssertArrays.makeIntegerVarArray12345();
		IntegerVarArray array2Int32 = AssertArrays.makeIntegerVarArray12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeIntegerVarArray12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		IntegerVarArray array = AssertArrays.makeIntegerVarArrayEmpty();
		IntegerVarArray copy = (IntegerVarArray) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArray12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copyGrow(3);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(4), IntegerValue.make(5), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0) }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		IntegerVarArray array = AssertArrays.makeIntegerVarArrayEmpty();
		IntegerVarArray copy = (IntegerVarArray) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArray12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArray12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(2), IntegerValue.make(3) }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 0);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(1), IntegerValue.make(2) }), copy);

		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 3);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(4), IntegerValue.make(5) }), copy);

		// partial copy with too large count
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(2), IntegerValue.make(3) }), copy);

		// partial copy with leading space
		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 1, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(2), IntegerValue.make(3) }), copy);

		// partial copy with leading space
		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(0) }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeIntegerVarArray12345();
		copy = (IntegerVarArray) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(0) }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeIntegerVarArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", AssertArrays.makeIntegerVarArray12345().toString());
	}

	public void testZeroElements() {
		IntegerVarArray array = IntegerVarArray.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IntegerVarArray.make(0), array);
		
		// zero all elements
		array = IntegerVarArray.make(1);
		array.storeIntegerVar(0, IntegerValue.make(1));
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] {IntegerValue.make(0)}), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.zeroElements();
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] {IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0)}), array);
		
		array = AssertArrays.makeIntegerVarArray12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] {IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0)}), array);

		// zero subset of elements
		array = AssertArrays.makeIntegerVarArray12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] {IntegerValue.make(1), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(0)}), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] {IntegerValue.make(1), IntegerValue.make(0), IntegerValue.make(0), IntegerValue.make(4), IntegerValue.make(5)}), array);

		array = AssertArrays.makeIntegerVarArray12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(IntegerVarArray.make(new IntegerValue[] {IntegerValue.make(1), IntegerValue.make(2), IntegerValue.make(3), IntegerValue.make(4), IntegerValue.make(0)}), array);

		// silently truncate from
		array = AssertArrays.makeIntegerVarArray12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeIntegerVarArray12345(), array);

		// extend count outside range
		array = AssertArrays.makeIntegerVarArray12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
}
