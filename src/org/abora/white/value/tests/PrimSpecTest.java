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

import org.abora.white.value.IntegerValue;
import org.abora.white.value.PrimSpec;

import junit.framework.TestCase;

public class PrimSpecTest extends TestCase {

	public PrimSpecTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(PrimSpecTest.class);
	}


	public void testIEEE32() {
		assertEquals(32, PrimSpec.iEEE32().bitCount());
	}

	public void testIEEE64() {
		assertEquals(64, PrimSpec.iEEE64().bitCount());
	}
	
	public void testIEEE() {
		assertSame(PrimSpec.iEEE32(), PrimSpec.iEEE(32));
		assertSame(PrimSpec.iEEE64(), PrimSpec.iEEE(64));
		try {
			PrimSpec.iEEE(48);
			fail();
		} catch (UnsupportedOperationException e) {
			// expected
		}
		try {
			PrimSpec.iEEE(-32);
			fail();
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}
	
	public void testInt8() {
		assertEquals(8, PrimSpec.int8().bitCount());
		assertTrue(PrimSpec.int8().isSigned());
	}

	public void testInt16() {
		assertEquals(16, PrimSpec.int16().bitCount());
		assertTrue(PrimSpec.int16().isSigned());
	}

	public void testInt32() {
		assertEquals(32, PrimSpec.int32().bitCount());
		assertTrue(PrimSpec.int32().isSigned());
	}

	public void testInt64() {
		assertEquals(64, PrimSpec.int64().bitCount());
		assertTrue(PrimSpec.int64().isSigned());
	}

	public void testSignedInteger() {
		assertSame(PrimSpec.int8(), PrimSpec.signedInteger(8));
		assertSame(PrimSpec.int16(), PrimSpec.signedInteger(16));
		assertSame(PrimSpec.int32(), PrimSpec.signedInteger(32));
		assertSame(PrimSpec.int64(), PrimSpec.signedInteger(64));
		try {
			PrimSpec.signedInteger(128);
			fail();
		} catch (UnsupportedOperationException e) {
			// expected
		}
		try {
			PrimSpec.signedInteger(-32);
			fail();
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testUInt8() {
		assertEquals(8, PrimSpec.uInt8().bitCount());
		assertFalse(PrimSpec.uInt8().isSigned());
	}

	public void testUInt16() {
		assertEquals(16, PrimSpec.uInt16().bitCount());
		assertFalse(PrimSpec.uInt16().isSigned());
	}

	public void testUInt32() {
		assertEquals(32, PrimSpec.uInt32().bitCount());
		assertFalse(PrimSpec.uInt32().isSigned());
	}

	public void testUnsignedInteger() {
		assertSame(PrimSpec.uInt8(), PrimSpec.unsignedInteger(8));
		assertSame(PrimSpec.uInt16(), PrimSpec.unsignedInteger(16));
		assertSame(PrimSpec.uInt32(), PrimSpec.unsignedInteger(32));
		try {
			PrimSpec.unsignedInteger(64);
			fail();
		} catch (UnsupportedOperationException e) {
			// expected
		}
		try {
			PrimSpec.unsignedInteger(-32);
			fail();
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}

	public void testToHold() {
		// Positive numbers
		assertSame(PrimSpec.int8(), PrimSpec.toHold(IntegerValue.make(0)));
		assertSame(PrimSpec.int8(), PrimSpec.toHold(IntegerValue.make(127)));
		assertSame(PrimSpec.uInt8(), PrimSpec.toHold(IntegerValue.make(128)));
		assertSame(PrimSpec.uInt8(), PrimSpec.toHold(IntegerValue.make(255)));
		assertSame(PrimSpec.int16(), PrimSpec.toHold(IntegerValue.make(256)));
		assertSame(PrimSpec.int16(), PrimSpec.toHold(IntegerValue.make(32767)));
		assertSame(PrimSpec.uInt16(), PrimSpec.toHold(IntegerValue.make(32768)));
		assertSame(PrimSpec.uInt16(), PrimSpec.toHold(IntegerValue.make(65535)));
		assertSame(PrimSpec.int32(), PrimSpec.toHold(IntegerValue.make(65536)));
		assertSame(PrimSpec.int32(), PrimSpec.toHold(IntegerValue.make(2147483647)));
		assertSame(PrimSpec.uInt32(), PrimSpec.toHold(IntegerValue.make(2147483648L)));
		assertSame(PrimSpec.uInt32(), PrimSpec.toHold(IntegerValue.make(4294967295L)));
		assertSame(PrimSpec.int64(), PrimSpec.toHold(IntegerValue.make(4294967296L)));
		assertSame(PrimSpec.int64(), PrimSpec.toHold(IntegerValue.make(9223372036854775807L)));
		//TODO IntegerValue once it uses BigInteger 

		// Negative numbers
		assertSame(PrimSpec.int8(), PrimSpec.toHold(IntegerValue.make(-1)));
		assertSame(PrimSpec.int8(), PrimSpec.toHold(IntegerValue.make(-128)));
		assertSame(PrimSpec.int16(), PrimSpec.toHold(IntegerValue.make(-129)));
		assertSame(PrimSpec.int16(), PrimSpec.toHold(IntegerValue.make(-32768)));
		assertSame(PrimSpec.int32(), PrimSpec.toHold(IntegerValue.make(-32769)));
		assertSame(PrimSpec.int32(), PrimSpec.toHold(IntegerValue.make(-2147483648)));
		assertSame(PrimSpec.int64(), PrimSpec.toHold(IntegerValue.make(-2147483649L)));
		assertSame(PrimSpec.int64(), PrimSpec.toHold(IntegerValue.make(-9223372036854775808L)));
		//TODO IntegerValue once it uses BigInteger 

	}
}
