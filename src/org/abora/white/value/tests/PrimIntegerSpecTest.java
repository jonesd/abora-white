/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.white.value.tests;

import junit.framework.TestCase;

import org.abora.white.value.IntegerValue;
import org.abora.white.value.PrimSpec;

public class PrimIntegerSpecTest extends TestCase {

	public PrimIntegerSpecTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(PrimIntegerSpecTest.class);
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
