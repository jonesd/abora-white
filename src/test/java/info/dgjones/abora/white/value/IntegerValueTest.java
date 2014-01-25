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

import java.math.BigInteger;

import junit.framework.TestCase;

import info.dgjones.abora.white.value.IEEE32Value;
import info.dgjones.abora.white.value.IntegerValue;

public class IntegerValueTest extends TestCase {

	public IntegerValueTest(String arg0) {
		super(arg0);
	}

	public void testOne() {
		assertEquals(1, IntegerValue.one().asInt32());
		assertSame(IntegerValue.one(), IntegerValue.one());
	}

	public void testZero() {
		assertEquals(0, IntegerValue.zero().asInt32());
		assertSame(IntegerValue.zero(), IntegerValue.zero());
	}
	
	public void testMakeLong() {
		assertEquals(0, IntegerValue.make(0).asInt64());
		assertEquals(Long.MAX_VALUE, IntegerValue.make(Long.MAX_VALUE).asInt64());
		assertEquals(Long.MIN_VALUE, IntegerValue.make(Long.MIN_VALUE).asInt64());
	}

	public void testMakeBigInteger() {
		assertEquals(0, IntegerValue.make(BigInteger.ZERO).asInt64());

		assertEquals(Long.MAX_VALUE, IntegerValue.make(BigInteger.valueOf(Long.MAX_VALUE)).asInt64());
		assertEquals(Long.MIN_VALUE, IntegerValue.make(BigInteger.valueOf(Long.MIN_VALUE)).asInt64());

		BigInteger large = BigInteger.valueOf(Long.MAX_VALUE).add(BigInteger.valueOf(1));
		assertEquals(large, IntegerValue.make(large).toBigInteger());
		BigInteger small = BigInteger.valueOf(Long.MIN_VALUE).subtract(BigInteger.valueOf(1));
		assertEquals(small, IntegerValue.make(small).toBigInteger());
	}
	
	public void testIsEqual() {
		IntegerValue value = IntegerValue.make(256);
		assertTrue(value.isEqual(value));
		assertTrue(IntegerValue.make(128).isEqual(IntegerValue.make(128)));
		assertFalse(value.isEqual(IEEE32Value.make(256.0f)));
	}

	public void testCompareTo() {
		assertEquals(0, IntegerValue.make(23).compareTo(IntegerValue.make(23)));
		assertEquals(-1, IntegerValue.make(22).compareTo(IntegerValue.make(23)));
		assertEquals(+1, IntegerValue.make(24).compareTo(IntegerValue.make(23)));
		
		try {
			IntegerValue.make(23).compareTo(IEEE32Value.make(23.0f));
			fail();
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testIsGE() {
		assertTrue(IntegerValue.make(23).isGE(IntegerValue.make(23)));
		assertFalse(IntegerValue.make(22).isGE(IntegerValue.make(23)));
		assertTrue(IntegerValue.make(24).isGE(IntegerValue.make(23)));
	}

	public void testIsLT() {
		assertFalse(IntegerValue.make(23).isLT(IntegerValue.make(23)));
		assertTrue(IntegerValue.make(22).isLT(IntegerValue.make(23)));
		assertFalse(IntegerValue.make(24).isLT(IntegerValue.make(23)));
	}

	public void testIsLE() {
		assertTrue(IntegerValue.make(23).isLE(IntegerValue.make(23)));
		assertTrue(IntegerValue.make(22).isLE(IntegerValue.make(23)));
		assertFalse(IntegerValue.make(24).isLE(IntegerValue.make(23)));
	}
	
	public void testActualHashForEqual() {
		assertEquals(0, IntegerValue.zero().actualHashForEqual());
		
		assertTrue(IntegerValue.make(123).actualHashForEqual() == IntegerValue.make(123).actualHashForEqual());
		assertFalse(IntegerValue.make(123).actualHashForEqual() == IntegerValue.make(124).actualHashForEqual());
	}

	public void testBitwiseAnd() {
		assertEquals(IntegerValue.make(4), IntegerValue.make(12).bitwiseAnd(IntegerValue.make(6)));
	}
	
	public void testBitwiseOr() {
		assertEquals(IntegerValue.make(14), IntegerValue.make(12).bitwiseOr(IntegerValue.make(6)));
	}

	public void testBitwiseXor() {
		assertEquals(IntegerValue.make(10), IntegerValue.make(12).bitwiseXor(IntegerValue.make(6)));
	}

	public void testDividedBy() {
		assertEquals(IntegerValue.make(4), IntegerValue.make(12).dividedBy(IntegerValue.make(3)));
	}

	public void testMinimum() {
		assertEquals(IntegerValue.make(6), IntegerValue.make(12).minimum(IntegerValue.make(6)));
		assertEquals(IntegerValue.make(6), IntegerValue.make(6).minimum(IntegerValue.make(12)));
	}

	public void testMaximum() {
		assertEquals(IntegerValue.make(12), IntegerValue.make(12).maximum(IntegerValue.make(6)));
		assertEquals(IntegerValue.make(12), IntegerValue.make(6).maximum(IntegerValue.make(12)));
	}

	public void testMinus() {
		assertEquals(IntegerValue.make(6), IntegerValue.make(12).minus(IntegerValue.make(6)));
		assertEquals(IntegerValue.make(-6), IntegerValue.make(6).minus(IntegerValue.make(12)));
	}

	public void testPlus() {
		assertEquals(IntegerValue.make(18), IntegerValue.make(12).plus(IntegerValue.make(6)));
	}

	public void testTimes() {
		assertEquals(IntegerValue.make(72), IntegerValue.make(12).times(IntegerValue.make(6)));
	}
}
