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
package info.dgjones.abora.white.value;

import junit.framework.TestCase;

import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.value.PrimSpec;

public class PrimIntegerSpecTest extends TestCase {

	public PrimIntegerSpecTest(String arg0) {
		super(arg0);
	}

	public void testCanHold() {
		// Unsigned integer
		assertTrue(PrimSpec.uInt8().canHold(IntegerValue.make(0)));
		assertTrue(PrimSpec.uInt8().canHold(IntegerValue.make(255)));
		assertFalse(PrimSpec.uInt8().canHold(IntegerValue.make(-1)));
		assertFalse(PrimSpec.uInt8().canHold(IntegerValue.make(256)));

		// Signed integer
		assertTrue(PrimSpec.int8().canHold(IntegerValue.make(0)));
		assertTrue(PrimSpec.int8().canHold(IntegerValue.make(127)));
		assertTrue(PrimSpec.int8().canHold(IntegerValue.make(-128)));
		assertFalse(PrimSpec.int8().canHold(IntegerValue.make(128)));
		assertFalse(PrimSpec.int8().canHold(IntegerValue.make(-129)));

		// Unbounded integer
		assertTrue(PrimSpec.integerVar().canHold(IntegerValue.make(0)));
//		TODO larger once integerVar is BigInteger
		assertTrue(PrimSpec.integerVar().canHold(IntegerValue.make(Long.MAX_VALUE)));
//		TODO smaller once integerVar is BigInteger
		assertTrue(PrimSpec.integerVar().canHold(IntegerValue.make(Long.MIN_VALUE)));
	}
	
	public void testCombine() {
		// Same
		assertSame(PrimSpec.uInt8(), PrimSpec.uInt8().combine(PrimSpec.uInt8()));
		assertSame(PrimSpec.int8(), PrimSpec.int8().combine(PrimSpec.int8()));

		// IntegerVar
		assertSame(PrimSpec.integerVar(), PrimSpec.integerVar().combine(PrimSpec.integerVar()));
		assertSame(PrimSpec.integerVar(), PrimSpec.uInt8().combine(PrimSpec.integerVar()));
		assertSame(PrimSpec.integerVar(), PrimSpec.integerVar().combine(PrimSpec.uInt8()));

		// Different bitCounts
		assertSame(PrimSpec.int16(), PrimSpec.int16().combine(PrimSpec.int8()));
		assertSame(PrimSpec.int16(), PrimSpec.int8().combine(PrimSpec.int16()));
		assertSame(PrimSpec.int16(), PrimSpec.int8().combine(PrimSpec.int16()));
		
		// Different sign
		assertSame(PrimSpec.int16(), PrimSpec.int8().combine(PrimSpec.uInt8()));
		assertSame(PrimSpec.int32(), PrimSpec.int16().combine(PrimSpec.uInt16()));
		assertSame(PrimSpec.int64(), PrimSpec.int32().combine(PrimSpec.uInt32()));
	}

	public void testValue() {
		IntegerValue value = PrimSpec.int32().value(123);
		assertEquals(123, value.asInt32());
	}

	public void testIsEqual() {
		assertTrue(PrimSpec.int32().isEqual(PrimSpec.int32()));
		assertFalse(PrimSpec.int32().isEqual(PrimSpec.int64()));
		assertFalse(PrimSpec.int32().isEqual(PrimSpec.uInt32()));		
		assertFalse(PrimSpec.int32().isEqual(PrimSpec.iEEE32()));		
	}
	
	public void testActualHashForEqual() {
		assertTrue(PrimSpec.int32().actualHashForEqual() == PrimSpec.int32().actualHashForEqual());
		assertFalse(PrimSpec.int32().actualHashForEqual() == PrimSpec.int64().actualHashForEqual());
		assertFalse(PrimSpec.int32().actualHashForEqual() == PrimSpec.uInt32().actualHashForEqual());
		assertFalse(PrimSpec.int32().actualHashForEqual() == PrimSpec.iEEE32().actualHashForEqual());
	}
}
