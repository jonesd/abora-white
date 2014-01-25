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
package info.dgjones.abora.white.collection.tables;

import junit.framework.TestCase;

import info.dgjones.abora.white.collection.tables.Pair;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.value.IntegerValue;

public class PairTest extends TestCase {

	public PairTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(PairTest.class);
	}

	public void testMake() {
		Pair p = Pair.make(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.zero(), p.left());
		assertEquals(IntegerValue.one(), p.right());
		
		// Null params
		try {
			Pair.make(null, IntegerValue.one());
			fail("null,1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			Pair.make(IntegerValue.zero(), null);
			fail("0,null");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			Pair.make(null, null);
			fail("null,null");
		} catch (IllegalArgumentException e) {
			// expected
		}
	}
	
	public void testPairWithNulls() {
		Pair p = Pair.pairWithNulls(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.zero(), p.left());
		assertEquals(IntegerValue.one(), p.right());

		// Null params
		p = Pair.pairWithNulls(null, IntegerValue.one());
		assertEquals(null, p.fetchLeft());
		assertEquals(IntegerValue.one(), p.fetchRight());

		p = Pair.pairWithNulls(IntegerValue.zero(), null);
		assertEquals(IntegerValue.zero(), p.fetchLeft());
		assertEquals(null, p.fetchRight());

		p = Pair.pairWithNulls(null, null);
		assertEquals(null, p.fetchLeft());
		assertEquals(null, p.fetchRight());
	}
	
	public void testLeft() {
		Pair p = Pair.pairWithNulls(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.zero(), p.left());
		assertEquals(IntegerValue.zero(), p.fetchLeft());

		p = Pair.pairWithNulls(null, IntegerValue.one());
		try {
			p.left();
			fail("null");
		} catch (AboraRuntimeException e) {
			// expected
		}
		assertEquals(null, p.fetchLeft());
	}

	public void testRight() {
		Pair p = Pair.pairWithNulls(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.one(), p.right());
		assertEquals(IntegerValue.one(), p.fetchRight());

		p = Pair.pairWithNulls(IntegerValue.zero(), null);
		try {
			p.right();
			fail("null");
		} catch (AboraRuntimeException e) {
			// expected
		}
		assertEquals(null, p.fetchRight());
	}
	
	public void testReversed() {
		Pair p = Pair.make(IntegerValue.zero(), IntegerValue.one());
		Pair reversed = p.reversed();
				
		assertEquals(IntegerValue.zero(), p.left());
		assertEquals(IntegerValue.one(), p.right());
		
		assertEquals(IntegerValue.one(), reversed.left());
		assertEquals(IntegerValue.zero(), reversed.right());

		// Null params
		p = Pair.pairWithNulls(null, IntegerValue.one());
		try {
			p.reversed();
			fail("null");
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testPrintOn() {
		Pair p = Pair.make(IntegerValue.zero(), IntegerValue.one());
		assertEquals("<0 , 1>", p.toString());		

		p = Pair.pairWithNulls(null, IntegerValue.one());
		assertEquals("<null , 1>", p.toString());		
	}
	
	public void testHashForEqual() {
		Pair p = Pair.make(IntegerValue.one(), IntegerValue.make(2));
		assertTrue(p.hashForEqual() != 0);
		assertTrue(p.hashForEqual() != p.left().hashCode());
		assertTrue(p.hashForEqual() != p.right().hashCode());
		
		p = Pair.pairWithNulls(null, IntegerValue.make(2));
		assertTrue(p.hashForEqual() != 0);

		p = Pair.pairWithNulls(IntegerValue.one(), null);
		assertTrue(p.hashForEqual() != 0);

		p = Pair.pairWithNulls(null, null);
		assertTrue(p.hashForEqual() != 0);
	}
	
	public void testIsEqual() {
		// Identity
		Pair p1 = Pair.make(IntegerValue.one(), IntegerValue.make(2));
		assertTrue(p1.isEqual(p1));
		
		// Equal
		Pair p2 = Pair.make(IntegerValue.one(), IntegerValue.make(2));
		assertTrue(p1.isEqual(p2));		
		
		// Equal with nulls
		p1 = Pair.pairWithNulls(null, IntegerValue.make(2));
		p2 = Pair.pairWithNulls(null, IntegerValue.make(2));
		assertTrue(p1.isEqual(p2));		

		p1 = Pair.pairWithNulls(IntegerValue.make(99), null);
		p2 = Pair.pairWithNulls(IntegerValue.make(99), null);
		assertTrue(p1.isEqual(p2));		

		p1 = Pair.pairWithNulls(null, null);
		p2 = Pair.pairWithNulls(null, null);
		assertTrue(p1.isEqual(p2));		

		// Not Equal
		p1 = Pair.make(IntegerValue.one(), IntegerValue.make(2));
		p2 = Pair.make(IntegerValue.one(), IntegerValue.make(3));
		assertFalse(p1.isEqual(p2));

		p1 = Pair.make(IntegerValue.make(2), IntegerValue.one());
		p2 = Pair.make(IntegerValue.make(3), IntegerValue.one());
		assertFalse(p1.isEqual(p2));

		// Not Equal with nulls
		p1 = Pair.pairWithNulls(null, IntegerValue.make(3));
		p2 = Pair.pairWithNulls(null, IntegerValue.make(2));
		assertFalse(p1.isEqual(p2));		

		p1 = Pair.pairWithNulls(IntegerValue.make(3), null);
		p2 = Pair.pairWithNulls(null, IntegerValue.make(3));
		assertFalse(p1.isEqual(p2));		
		
		// Not Equal with non-pairs
		assertFalse(p1.isEqual(IntegerValue.make(3)));
		assertFalse(p1.isEqual(null));				
	}
}
