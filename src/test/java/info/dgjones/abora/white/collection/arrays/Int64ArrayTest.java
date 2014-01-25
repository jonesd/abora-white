/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.white.collection.arrays;

import java.util.Arrays;

import junit.framework.TestCase;

import info.dgjones.abora.white.collection.arrays.IEEE32Array;
import info.dgjones.abora.white.collection.arrays.IEEE64Array;
import info.dgjones.abora.white.collection.arrays.Int64Array;
import info.dgjones.abora.white.collection.arrays.IntegerVarArray;
import info.dgjones.abora.white.value.IEEE32Value;
import info.dgjones.abora.white.value.IEEE64Value;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.value.PrimIntegerSpec;

public class Int64ArrayTest extends TestCase {

	public Int64ArrayTest(String arg0) {
		super(arg0);
	}

	public void testMakeCount() {
		Int64Array array = Int64Array.make(0);
		assertEquals(0, array.count());

		array = Int64Array.make(1);
		AssertArrays.assertEquals(1, array.count());
		AssertArrays.assertEquals(0, array.int64At(0));
		
		try {
			Int64Array.make(-1);
			fail("-1");
		} catch (NegativeArraySizeException e) {
			//expected
		}
	}

	public void testMake() {
		Int64Array array = Int64Array.make(AssertArrays.makeInt64ArrayEmpty());
		assertEquals(0, array.count());

		array = Int64Array.make(AssertArrays.makeInt64Array12345());
		AssertArrays.assertEquals(5, array.count());
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12345(), array);

		array = Int64Array.make(7, AssertArrays.makeInt64Array12345());
		AssertArrays.assertEquals(7, array.count());
		AssertArrays.assertEquals(Int64Array.make(new long[]{1,2,3,4,5,0,0}), array);

		array = Int64Array.make(7, AssertArrays.makeInt64Array12345(), 1, 2, 5);
		AssertArrays.assertEquals(7, array.count());
		AssertArrays.assertEquals(Int64Array.make(new long[]{0,0,0,0,0,2,3}), array);		

		try {
			Int64Array.make(4, AssertArrays.makeInt64Array12345());
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}
	
	public void testMakeBuffer() {
		Int64Array array = Int64Array.make(new long[] {});
		assertEquals(0, array.count());

		array = Int64Array.make(new long[] {1, 2});
		assertEquals(2, array.count());
		assertEquals(1, array.int64At(0));
		assertEquals(2, array.int64At(1));		
	}

	public void testInt32At() {
		Int64Array a = Int64Array.make(new long[] { 0, 1, -2, 3 });

		AssertArrays.assertEquals(0, a.int64At(0));
		AssertArrays.assertEquals(1, a.int64At(1));
		AssertArrays.assertEquals(-2, a.int64At(2));
		AssertArrays.assertEquals(3, a.int64At(3));

		try {
			a.int64At(-1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
		try {
			a.int64At(4);
			fail("4");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testInt32AtEmpty() {
		Int64Array a = Int64Array.make(0);
		try {
			a.int64At(0);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// OutOfBounds
		}
	}

	public void testIntegerAt() {
		Int64Array a = Int64Array.make(new long[] { 0, 1, -2, 3 });

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
		Int64Array a = Int64Array.make(new long[] { 0, 1, -2, 3 });

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
		AssertArrays.assertEquals(0, AssertArrays.makeInt64ArrayEmpty().count());
		AssertArrays.assertEquals(1, Int64Array.make(new long[] { 0 }).count());
		AssertArrays.assertEquals(2, Int64Array.make(new long[] { 0, 1 }).count());
	}

	public void testStoreInt32() {
		Int64Array empty = Int64Array.make(0);
		Int64Array tri = Int64Array.make(3);

		tri.storeInt64(0, Long.MIN_VALUE);
		assertEquals(tri.int64At(0), Long.MIN_VALUE);
		tri.storeInt64(1, (int) 1);
		assertEquals(tri.int64At(1), 1);
		tri.storeInt64(2, Long.MAX_VALUE);
		assertEquals(tri.int64At(2), Long.MAX_VALUE);

		try {
			tri.storeInt64(-1, (int) 1);
			fail("-1");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			tri.storeInt64(3, (int) 1);
			fail("3");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			empty.storeInt64(0, (int) 1);
			fail("0");
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testStoreInteger() {
		Int64Array empty = Int64Array.make(0);
		Int64Array tri = Int64Array.make(3);

		// Store integer values within spec
		tri.storeInteger(0, IntegerValue.make(Long.MIN_VALUE));
		assertTrue(tri.int64At(0) == Long.MIN_VALUE);
		tri.storeInteger(1, IntegerValue.make(1));
		assertEquals(tri.int64At(1), 1);
		tri.storeInteger(2, IntegerValue.make(Long.MAX_VALUE));
		assertTrue(tri.int64At(2) == Long.MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeInteger(0, IntegerValue.make(Long.MIN_VALUE).minus(IntegerValue.one()));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeInteger(2, IntegerValue.make(Long.MAX_VALUE).plus(IntegerValue.one()));
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
		Int64Array empty = Int64Array.make(0);
		Int64Array tri = Int64Array.make(3);

		// Store integer values within spec
		tri.storeValue(0, IntegerValue.make(Long.MIN_VALUE));
		assertTrue(tri.int64At(0) == Long.MIN_VALUE);
		tri.storeValue(1, IntegerValue.make(1));
		assertEquals(tri.int64At(1), 1);
		tri.storeValue(2, IntegerValue.make(Long.MAX_VALUE));
		assertTrue(tri.int64At(2) == Long.MAX_VALUE);

		// Store integer values outside of spec
		try {
			tri.storeValue(0, IntegerValue.make(Long.MIN_VALUE).minus(IntegerValue.one()));
			fail("MIN_VALUE - 1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			tri.storeValue(2, IntegerValue.make(Long.MAX_VALUE).plus(IntegerValue.one()));
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
		
		// Null value
		try {tri.storeValue(0, null);
			fail("null");
		} catch (NullPointerException e) {
			// expected
		}
	}

	public void testStoreAll() {
		Int64Array array = Int64Array.make(0);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(Int64Array.make(0), array);

		array = Int64Array.make(1);
		array.storeAll(IntegerValue.make(1));
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1 }), array);

		array = Int64Array.make(3);
		array.storeAll(IntegerValue.make(2));
		AssertArrays.assertEquals(Int64Array.make(new long[] { 2, 2, 2 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		assertEquals(Int64Array.make(new long[] { 1, 9, 9, 4, 5 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeAll(null, 2, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 0, 0, 4, 5 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeAll(IntegerValue.make(9), -1, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 9, 9, 9, 9 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeAll(IntegerValue.make(9), 2);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 9, 9, 3, 4, 5 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeAll(IntegerValue.make(9), 0, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2, 3, 4, 5 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeAll(IntegerValue.make(9), 2, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 9, 9, 4, 5 }), array);

		// Store outside of array bounds
		array = AssertArrays.makeInt64Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 6);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeInt64Array12345();
		try {
			array.storeAll(IntegerValue.make(9), 4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// Store incompatible type
		array = AssertArrays.makeInt64Array12345();
		try {
			array.storeAll(IEEE32Value.make(9.9f), 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeInt64Array12345();
		try {
			array.storeAll(IntegerValue.make(Long.MAX_VALUE).plus(IntegerValue.one()), 1);
			fail();
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testStoreMany() {
		// empty
		Int64Array array = AssertArrays.makeInt64ArrayEmpty();
		array.storeMany(0, AssertArrays.makeInt64ArrayEmpty());
		AssertArrays.assertEquals(AssertArrays.makeInt64ArrayEmpty(), array);

		// simple
		array = AssertArrays.makeInt64Array12345();
		array.storeMany(0, AssertArrays.makeInt64Array12321());
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12321(), array);

		array = AssertArrays.makeInt64Array12345();
		array.storeMany(0, AssertArrays.makeInt64Array12321());
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12321(), array);

		array = AssertArrays.makeInt64Array12321();
		array.storeMany(1, Int64Array.make(new long[] { 8, 7, 6, 5, 4, 3, 3 }), 2, 3);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 5, 4, 2, 1 }), array);

		array = AssertArrays.makeInt64Array12321();
		array.storeMany(1, Int64Array.make(new long[] { 8, 7 }));
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 8, 7, 2, 1 }), array);

		// Store incompatible type
		array = AssertArrays.makeInt64Array12321();
		try {
			array.storeMany(1, IEEE64Array.make(new double[] { 8.8, 7.7 }));
			fail();
		} catch (ClassCastException e) {
			// expected
		}

		// attempt to copy beyond this extent
		array = AssertArrays.makeInt64Array12345();
		try {
			array.storeMany(1, AssertArrays.makeInt64Array12321());
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// insufficient source elements
		array = AssertArrays.makeInt64Array12345();
		try {
			array.storeMany(0, AssertArrays.makeInt64Array12321(), 2, 4);
			fail();
		} catch (IndexOutOfBoundsException e) {
			//expected
		}

		// Store integer values outside of spec
		array = AssertArrays.makeInt64Array12321();
		try {
			array.storeMany(1, IntegerVarArray.make(new IntegerValue[] { IntegerValue.make(Long.MAX_VALUE).plus(IntegerValue.one()) }));
			fail("MAX_VALUE + 1");
		} catch (IllegalArgumentException e) {
			//expected
		}
	}

	public void testCopyToBuffer() {
		Int64Array array = AssertArrays.makeInt64Array12345();
		long[] out = new long[3];
		array.copyToBuffer(out, 3, 1);
		assertTrue(Arrays.equals(out, new long[] { 2, 3, 4 }));

		array = AssertArrays.makeInt64Array12345();
		out = new long[1];
		array.copyToBuffer(out, -1, 0);
		assertTrue(Arrays.equals(out, new long[] { 1 }));

		array = AssertArrays.makeInt64Array12345();
		out = new long[1];
		array.copyToBuffer(out, -1, 4);
		assertTrue(Arrays.equals(out, new long[] { 5 }));

		array = AssertArrays.makeInt64Array12345();
		out = new long[3];
		array.copyToBuffer(out, 3, 0);
		assertTrue(Arrays.equals(out, new long[] { 1, 2, 3 }));

		array = AssertArrays.makeInt64Array12345();
		out = new long[3];
		array.copyToBuffer(out, -1, 2);
		assertTrue(Arrays.equals(out, new long[] { 3, 4, 5 }));

		array = AssertArrays.makeInt64Array12345();
		out = new long[3];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new long[] { 4, 5, 0 }));

		array = AssertArrays.makeInt64Array12345();
		out = new long[0];
		array.copyToBuffer(out, -1, 3);
		assertTrue(Arrays.equals(out, new long[] {
		}));

		array = AssertArrays.makeInt64Array12345();
		out = new long[3];
		try {
			array.copyToBuffer(out, -1, -1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		array = AssertArrays.makeInt64Array12345();
		out = new long[3];
		try {
			array.copyToBuffer(out, 1, 5);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

	}

	public void testIsEqual() {
		long[] intsE = new long[] {};
		long[] ints1 = new long[] { 1 };
		long[] ints12 = new long[] { 1, 2 };
		long[] ints11 = new long[] { 1, 1 };

		// Equal matches
		assertTrue(Int64Array.make(intsE).isEqual(Int64Array.make(intsE)));
		assertTrue(Int64Array.make(ints1).isEqual(Int64Array.make(ints1)));
		assertTrue(Int64Array.make(ints12).isEqual(Int64Array.make(ints12)));
		assertTrue(Int64Array.make(ints1).isEqual(AssertArrays.makeInt64Array1()));
		
		// Unequal compatible matches
		assertFalse(Int64Array.make(ints11).isEqual(Int64Array.make(ints12)));
		assertFalse(Int64Array.make(intsE).isEqual(Int64Array.make(ints12)));
		assertFalse(Int64Array.make(ints12).isEqual(Int64Array.make(intsE)));

		// single value
		assertFalse(Int64Array.make(ints1).isEqual(IntegerValue.make(1)));
		
		// incompatible array
		try {
			assertFalse(Int64Array.make(ints1).isEqual(AssertArrays.makeIEEE32Array1()));
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testIndexOf() {
		int index = AssertArrays.makeInt64ArrayEmpty().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array1().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt64Array1().indexOf(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12345().indexOf(IntegerValue.make(1), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt64Array12345().indexOf(IntegerValue.make(1), 0, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12345().indexOf(IntegerValue.make(1), 1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12345().indexOf(IntegerValue.make(5), 0, 1);
		assertEquals(4, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), 0, 2);
		assertEquals(3, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(1), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -1, -2);
		assertEquals(1, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -1, -3);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -3, -1);
		assertEquals(1, index);

		index = AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeInt64Array12321().indexOf(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeInt64Array12321().indexOf(IEEE64Value.make(2.0f), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexPast() {
		int index = AssertArrays.makeInt64ArrayEmpty().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array1().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array1().indexPast(IntegerValue.make(1), 0, 0);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12345().indexPast(IntegerValue.make(1), 0, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt64Array12345().indexPast(IntegerValue.make(1), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeInt64Array12345().indexPast(IntegerValue.make(1), 1, 1);
		assertEquals(1, index);

		index = AssertArrays.makeInt64Array12345().indexPast(IntegerValue.make(5), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt64Array12345().indexPast(IntegerValue.make(5), 3, 1);
		assertEquals(3, index);

		index = AssertArrays.makeInt64Array12345().indexPast(IntegerValue.make(5), 4, 1);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), 0, 1);
		assertEquals(0, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), 0, 2);
		assertEquals(2, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(1), -1, -1);
		assertEquals(3, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -1, -1);
		assertEquals(4, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -1, 1);
		assertEquals(4, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -1, 2);
		assertEquals(-1, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -1, -2);
		assertEquals(2, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -1, -3);
		assertEquals(0, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -3, -1);
		assertEquals(2, index);

		index = AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(1), -1, 1);
		assertEquals(-1, index);

		try {
			AssertArrays.makeInt64Array12321().indexPast(IntegerValue.make(2), -6, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		try {
			AssertArrays.makeInt64Array12321().indexPast(IEEE64Value.make(2.0), -3, 1);
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIndexOfElements() {
		// empty
		Int64Array array = AssertArrays.makeInt64ArrayEmpty();
		Int64Array search = AssertArrays.makeInt64Array1();
		assertEquals(-1, array.indexOfElements(search));

		array = AssertArrays.makeInt64ArrayEmpty();
		search = AssertArrays.makeInt64ArrayEmpty();
		assertEquals(-1, array.indexOfElements(search));

		// TODO skip zero length other?
		//		array = makeInt64Array12345();
		//		search = makeInt64ArrayEmpty();
		//		assertEquals(-1, array.indexOfElements(search));

		// forward search
		array = Int64Array.make(new long[] { 1, 2, 3, 1, 2 });
		search = Int64Array.make(new long[] { 1, 2 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(3, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		array = AssertArrays.makeInt64Array12321();
		search = Int64Array.make(new long[] { 4, 9, 2, 8 });
		assertEquals(1, array.indexOfElements(search, 1, 2, 0, 1));
		assertEquals(3, array.indexOfElements(search, 1, 2, 0, 2));
		assertEquals(-1, array.indexOfElements(search, 1, 2, 0, 3));

		// forward search with compatible int array
		array = Int64Array.make(new long[] { 1, 2, 3, 1, 2 });
		Int64Array searchInt32 = Int64Array.make(new long[] { 1, 2 });
		assertEquals(0, array.indexOfElements(searchInt32));
		assertEquals(3, array.indexOfElements(searchInt32, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(searchInt32, -1, 0, 0, 3));

		// reverse search		
		array = Int64Array.make(new long[] { 1, 2, 3, 1, 2 });
		search = Int64Array.make(new long[] { 1, 2 });
		assertEquals(3, array.indexOfElements(search, -1, 0, -2, -1));
		assertEquals(0, array.indexOfElements(search, -1, 0, -2, -2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, -2, -3));

		// overlapping search
		// TODO should this succeed?
		array = Int64Array.make(new long[] { 1, 1, 1 });
		search = Int64Array.make(new long[] { 1, 1 });
		assertEquals(0, array.indexOfElements(search));
		assertEquals(1, array.indexOfElements(search, -1, 0, 0, 2));
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 3));

		// nth == 0 immediate fail
		array = AssertArrays.makeInt64Array12321();
		search = Int64Array.make(new long[] { 1 });
		assertEquals(-1, array.indexOfElements(search, -1, 0, 0, 0));

		// overflowing otherCount
		array = AssertArrays.makeInt64Array12321();
		search = Int64Array.make(new long[] { 1, 2 });
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
		array = AssertArrays.makeInt64Array12321();
		search = Int64Array.make(new long[] { 1, 2 });
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
		array = Int64Array.make(new long[] { 1, 2, 3, 1, 2 });
		IEEE32Array searchIEEE = IEEE32Array.make(new float[] { 1.0f, 2.0f });
		try {
			assertEquals(0, array.indexOfElements(searchIEEE));
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testAddElements() {
		Int64Array array = AssertArrays.makeInt64ArrayEmpty();
		array.addElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		assertEquals(AssertArrays.makeInt64ArrayEmpty(), array);

		array = AssertArrays.makeInt64Array1();
		array.addElements(0, Int64Array.make(new long[] { 9 }), -1, 0);
		assertEquals(Int64Array.make(new long[] { 10 }), array);

		array = Int64Array.make(5);
		array.addElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		assertEquals(AssertArrays.makeInt64Array12321(), array);

		array = AssertArrays.makeInt64Array12345();
		array.addElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		assertEquals(Int64Array.make(new long[] { 2, 4, 6, 6, 6 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.addElements(2, AssertArrays.makeInt64Array12321(), -1, 0);
		assertEquals(Int64Array.make(new long[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.addElements(2, AssertArrays.makeInt64Array12321(), -1);
		assertEquals(Int64Array.make(new long[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.addElements(2, AssertArrays.makeInt64Array12321());
		assertEquals(Int64Array.make(new long[] { 1, 2, 4, 6, 8 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.addElements(2, AssertArrays.makeInt64Array12321(), 2, 1);
		assertEquals(Int64Array.make(new long[] { 1, 2, 5, 7, 5 }), array);

		// element arithmetic overflows
		final long large = 0x6000000000000000L;
		array = Int64Array.make(new long[] {large, -large, -large});
		array.addElements(0, Int64Array.make(new long[] {large, large, -large}));
		final long result = 4611686018427387904L;
		assertEquals(Int64Array.make(new long[]{-result, 0, result}), array);

		// compatible array types
		array = AssertArrays.makeInt64Array12345();
		array.addElements(0, AssertArrays.makeInt32Array12321(), -1, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[]{2, 4, 6, 6, 6}), array);

		array = AssertArrays.makeInt64Array12345();
		try {
			array.addElements(2, AssertArrays.makeInt64Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeInt64ArrayEmpty();
		try {
			array.addElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testSubtractElements() {
		Int64Array array = AssertArrays.makeInt64ArrayEmpty();
		array.subtractElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		AssertArrays.assertEquals(AssertArrays.makeInt64ArrayEmpty(), array);

		array = AssertArrays.makeInt64Array1();
		array.subtractElements(0, Int64Array.make(new long[] { 9 }), -1, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[] { -8 }), array);

		array = Int64Array.make(5);
		array.subtractElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[] { -1, -2, -3, -2, -1 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.subtractElements(0, AssertArrays.makeInt64Array12321(), -1, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 0, 0, 0, 2, 4 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.subtractElements(2, AssertArrays.makeInt64Array12321(), -1, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.subtractElements(2, AssertArrays.makeInt64Array12321(), -1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.subtractElements(2, AssertArrays.makeInt64Array12321());
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2, 2, 2, 2 }), array);

		array = AssertArrays.makeInt64Array12345();
		array.subtractElements(2, AssertArrays.makeInt64Array12321(), 2, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2, 1, 1, 5 }), array);

		// element arithmetic overflows
		final long large = 0x6000000000000000L;
		array = Int64Array.make(new long[] {large, large, -large});
		array.subtractElements(0, Int64Array.make(new long[] {-large, large, large}));
		final long result = 4611686018427387904L;
		assertEquals(Int64Array.make(new long[]{-result, 0, result}), array);

		// compatible array types
		array = AssertArrays.makeInt64Array12345();
		array.subtractElements(0, AssertArrays.makeInt8Array12321(), -1, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[]{0, 0, 0, 2, 4}), array);

		// extend count beyond end of array
		array = AssertArrays.makeInt64Array12345();
		try {
			array.subtractElements(2, AssertArrays.makeInt64Array12321(), 4, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// incompatible arrays
		array = AssertArrays.makeInt64ArrayEmpty();
		try {
			array.subtractElements(0, AssertArrays.makeIEEE32Array12321(), -1, 0);
			fail("ieee");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCompare() {
		// same
		Int64Array array1 = AssertArrays.makeInt64ArrayEmpty();
		Int64Array array2 = AssertArrays.makeInt64ArrayEmpty();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeInt64Array12345();
		array2 = AssertArrays.makeInt64Array12345();
		assertEquals(0, array1.compare(array2));

		array1 = AssertArrays.makeInt64Array12321();
		array2 = AssertArrays.makeInt64Array12321();
		assertEquals(0, array1.compare(array2));

		// different
		array1 = AssertArrays.makeInt64Array12321();
		array2 = AssertArrays.makeInt64Array12345();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeInt64Array12345();
		array2 = AssertArrays.makeInt64Array12321();
		assertEquals(1, array1.compare(array2));

		// auto-filling with 0
		array1 = AssertArrays.makeInt64ArrayEmpty();
		array2 = AssertArrays.makeInt64Array1();
		assertEquals(-1, array1.compare(array2));

		array1 = AssertArrays.makeInt64Array1();
		array2 = AssertArrays.makeInt64ArrayEmpty();
		assertEquals(1, array1.compare(array2));

		array1 = Int64Array.make(new long[] { 0, 0 });
		array2 = Int64Array.make(new long[] { 0 });
		assertEquals(0, array1.compare(array2));

		array1 = Int64Array.make(new long[]{1, -1});
		array2 = Int64Array.make(new long[]{1});
		assertEquals(-1, array1.compare(array2));

		// compare sub-regions		
		array1 = AssertArrays.makeInt64Array12321();
		array2 = AssertArrays.makeInt64Array12345();
		assertEquals(1, array1.compare(array2, 2, 2, 1));

		array1 = AssertArrays.makeInt64Array12321();
		array2 = AssertArrays.makeInt64Array12345();
		assertEquals(1, array1.compare(array2, 2, 2));

		array1 = AssertArrays.makeInt64Array12321();
		array2 = AssertArrays.makeInt64Array12345();
		assertEquals(0, array1.compare(array2, 1, 4));

		// trim down count
		array1 = AssertArrays.makeInt64Array12321();
		array2 = AssertArrays.makeInt64Array12345();
		assertEquals(-1, array1.compare(array2, 10));

		// compare near minimum held value
		array1 = Int64Array.make(new long[] { Long.MIN_VALUE});
		array2 = Int64Array.make(new long[] { Long.MAX_VALUE});
		assertEquals(-1, array1.compare(array2));

		// different array types
		array1 = AssertArrays.makeInt64Array12345();
		Int64Array array2Int32 = AssertArrays.makeInt64Array12345();
		assertEquals(0, array1.compare(array2Int32));

		// incompatible array types
		array1 = AssertArrays.makeInt64Array12345();
		IEEE32Array array2IEEE32 = AssertArrays.makeIEEE32Array12345();
		try {
			array1.compare(array2IEEE32);
			fail("ieee32");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testCopyGrow() {
		Int64Array array = AssertArrays.makeInt64ArrayEmpty();
		Int64Array copy = (Int64Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeInt64ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copyGrow(0);
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12345(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copyGrow(3);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2, 3, 4, 5, 0, 0, 0 }), copy);
		assertNotSame(array, copy);
	}

	public void testCopy() {
		// full copy
		Int64Array array = AssertArrays.makeInt64ArrayEmpty();
		Int64Array copy = (Int64Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeInt64ArrayEmpty(), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy();
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12345(), copy);
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12345(), array);
		assertNotSame(array, copy);

		// partial copy
		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 2, 3 }), copy);
		assertNotSame(array, copy);

		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 0);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 1, 2 }), copy);

		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 3);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 4, 5 }), copy);

		// partial copy with too large count
		array = AssertArrays.makeInt64Array12345();
		try {
			array.copy(5, 1);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}

		// partial copy with trailing space
		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 1, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 0, 2, 3 }), copy);

		// partial copy with leading space
		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 1, 0, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 2, 3, 0 }), copy);

		// partial copy with leading and trailing space
		array = AssertArrays.makeInt64Array12345();
		copy = (Int64Array) array.copy(2, 1, 2, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] { 0, 0, 2, 3, 0 }), copy);
	}

	public void testToString() {
		assertEquals("[empty]", AssertArrays.makeInt64ArrayEmpty().toString());
		assertEquals("[1 2 3 4 5]", AssertArrays.makeInt64Array12345().toString());
	}

	public void testZeroElements() {
		Int64Array array = Int64Array.make(0);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int64Array.make(0), array);
		
		// zero all elements
		array = Int64Array.make(1);
		array.storeInt64(0, 1);
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int64Array.make(new long[] {0}), array);

		array = AssertArrays.makeInt64Array12345();
		array.zeroElements();
		AssertArrays.assertEquals(Int64Array.make(new long[] {0, 0, 0, 0, 0}), array);
		
		array = AssertArrays.makeInt64Array12345();
		array.zeroElements(0, -1);
		AssertArrays.assertEquals(Int64Array.make(new long[] {0, 0, 0, 0, 0}), array);

		// zero subset of elements
		array = AssertArrays.makeInt64Array12345();
		array.zeroElements(1, -1);
		AssertArrays.assertEquals(Int64Array.make(new long[] {1, 0, 0, 0, 0}), array);

		array = AssertArrays.makeInt64Array12345();
		array.zeroElements(1, 2);
		AssertArrays.assertEquals(Int64Array.make(new long[] {1, 0, 0, 4, 5}), array);

		array = AssertArrays.makeInt64Array12345();
		array.zeroElements(4, 1);
		AssertArrays.assertEquals(Int64Array.make(new long[] {1, 2, 3, 4, 0}), array);

		// silently truncate from
		array = AssertArrays.makeInt64Array12345();
		//TODO should this actually throw an exception?
		array.zeroElements(5, -1);
		AssertArrays.assertEquals(AssertArrays.makeInt64Array12345(), array);

		// extend count outside range
		array = AssertArrays.makeInt64Array12345();
		try {
			array.zeroElements(4, 2);
			fail();
		} catch (IndexOutOfBoundsException e) {
			// expected
		}
	}

	public void testBitCount() {
		Int64Array array = AssertArrays.makeInt64Array1(); 
		assertEquals(((PrimIntegerSpec)array.spec()).bitCount(), Math.abs(array.bitCount()));
		assertTrue(array.bitCount() < 0);
	}
}
