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

import org.abora.white.collection.arrays.PrimArray;
import org.abora.white.value.IEEE32Value;
import org.abora.white.value.IntegerValue;
import org.abora.white.value.PrimFloatValue;
import org.abora.white.value.PrimSpec;

public class PrimSpecTest extends TestCase {
	//TODO duplicate spec - move to some shared place
	private static final float DIFF = 0.000001f;

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
		assertSame(PrimSpec.integerVar(), PrimSpec.toHold(IntegerValue.make(9223372036854775807L).plus(IntegerValue.make(1))));

		// Negative numbers
		assertSame(PrimSpec.int8(), PrimSpec.toHold(IntegerValue.make(-1)));
		assertSame(PrimSpec.int8(), PrimSpec.toHold(IntegerValue.make(-128)));
		assertSame(PrimSpec.int16(), PrimSpec.toHold(IntegerValue.make(-129)));
		assertSame(PrimSpec.int16(), PrimSpec.toHold(IntegerValue.make(-32768)));
		assertSame(PrimSpec.int32(), PrimSpec.toHold(IntegerValue.make(-32769)));
		assertSame(PrimSpec.int32(), PrimSpec.toHold(IntegerValue.make(-2147483648)));
		assertSame(PrimSpec.int64(), PrimSpec.toHold(IntegerValue.make(-2147483649L)));
		assertSame(PrimSpec.int64(), PrimSpec.toHold(IntegerValue.make(-9223372036854775808L)));
		assertSame(PrimSpec.integerVar(), PrimSpec.toHold(IntegerValue.make(-9223372036854775808L).minus(IntegerValue.make(1))));
	}
	
	public void testArray() {
		// Different types
		PrimArray array = PrimSpec.int8().array(3);
		assertSame(PrimSpec.int8(), array.spec());
		assertEquals(3, array.count());
		assertEquals(0, ((IntegerValue)array.getValue(0)).intValue());		
		assertEquals(0, ((IntegerValue)array.getValue(1)).intValue());		
		assertEquals(0, ((IntegerValue)array.getValue(2)).intValue());		

		array = PrimSpec.iEEE32().array(3);
		assertSame(PrimSpec.iEEE32(), array.spec());
		assertEquals(3, array.count());

		array = PrimSpec.pointer().array(3);
		assertSame(PrimSpec.pointer(), array.spec());
		assertEquals(3, array.count());
	}
	
	public void testArrayEmpty() {
		PrimArray array = PrimSpec.int8().array();
		assertSame(PrimSpec.int8(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.int16().array();
		assertSame(PrimSpec.int16(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.int32().array();
		assertSame(PrimSpec.int32(), array.spec());
		assertEquals(0, array.count());
		
		array = PrimSpec.int64().array();
		assertSame(PrimSpec.int64(), array.spec());
		assertEquals(0, array.count());
		
		array = PrimSpec.uInt8().array();
		assertSame(PrimSpec.uInt8(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.uInt16().array();
		assertSame(PrimSpec.uInt16(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.uInt32().array();
		assertSame(PrimSpec.uInt32(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.integerVar().array();
		assertSame(PrimSpec.integerVar(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.iEEE32().array();
		assertSame(PrimSpec.iEEE32(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.iEEE64().array();
		assertSame(PrimSpec.iEEE64(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.pointer().array();
		assertSame(PrimSpec.pointer(), array.spec());
		assertEquals(0, array.count());

		array = PrimSpec.sharedPointer().array();
		assertSame(PrimSpec.sharedPointer(), array.spec());
		assertEquals(0, array.count());
	}
	
	public void testArrayWith() {
		PrimArray array = PrimSpec.int8().arrayWith(IntegerValue.make(64));
		assertEquals(1, array.count());
		IntegerValue integerValue = (IntegerValue)array.fetchValue(0);
		assertEquals(64, integerValue.intValue());

		array = PrimSpec.iEEE32().arrayWith(IEEE32Value.make(1.1f));
		assertEquals(1, array.count());
		IEEE32Value floatValue = (IEEE32Value)array.fetchValue(0);
		assertEquals(1.1f, floatValue.asIEEE32(), 0.0f);
		
		//TODO try pointer spec as well
	}
	
	public void testArrayWithTwo() {
		PrimArray array = PrimSpec.int8().arrayWithTwo(IntegerValue.make(64), IntegerValue.make(32));
		assertEquals(2, array.count());
		IntegerValue integerValue = (IntegerValue)array.fetchValue(0);
		assertEquals(64, integerValue.intValue());
		integerValue = (IntegerValue)array.fetchValue(1);
		assertEquals(32, integerValue.intValue());

		array = PrimSpec.iEEE32().arrayWithTwo(IEEE32Value.make(1.1f), IEEE32Value.make(2.2f));
		assertEquals(2, array.count());
		IEEE32Value floatValue = (IEEE32Value)array.fetchValue(0);
		assertEquals(1.1f, floatValue.asIEEE32(), 0.0f);
		floatValue = (IEEE32Value)array.fetchValue(1);
		assertEquals(2.2f, floatValue.asIEEE32(), 0.0f);
		
		//TODO try pointer spec as well
	}

	public void testArrayWithThree() {
		PrimArray array = PrimSpec.int8().arrayWithThree(IntegerValue.make(64), IntegerValue.make(32), IntegerValue.make(16));
		assertEquals(3, array.count());
		IntegerValue integerValue = (IntegerValue)array.fetchValue(0);
		assertEquals(64, integerValue.intValue());
		integerValue = (IntegerValue)array.fetchValue(1);
		assertEquals(32, integerValue.intValue());
		integerValue = (IntegerValue)array.fetchValue(2);
		assertEquals(16, integerValue.intValue());

		array = PrimSpec.iEEE32().arrayWithThree(IEEE32Value.make(1.1f), IEEE32Value.make(2.2f), IEEE32Value.make(3.3f));
		assertEquals(3, array.count());
		IEEE32Value floatValue = (IEEE32Value)array.fetchValue(0);
		assertEquals(1.1f, floatValue.asIEEE32(), 0.0f);
		floatValue = (IEEE32Value)array.fetchValue(1);
		assertEquals(2.2f, floatValue.asIEEE32(), 0.0f);
		floatValue = (IEEE32Value)array.fetchValue(2);
		assertEquals(3.3f, floatValue.asIEEE32(), 0.0f);
		
		//TODO try pointer spec as well
	}

	public void testArrayFromBuffer() {
		// empty
//TODO readd when finished off array subclasses
//		PrimArray array = PrimSpec.int8().arrayFromBuffer(new byte[0]);
//		assertSame(PrimSpec.int8(), array.spec());
//		assertEquals(0, array.count());
//
//		// good elements
//		array = PrimSpec.int8().arrayFromBuffer(new byte[]{0, 1, 2});
//		assertSame(PrimSpec.int8(), array.spec());
//		assertEquals(3, array.count());
//		assertEquals(0, ((IntegerValue)array.getValue(0)).intValue());		
//		assertEquals(1, ((IntegerValue)array.getValue(1)).intValue());		
//		assertEquals(2, ((IntegerValue)array.getValue(2)).intValue());		

		PrimArray array = PrimSpec.iEEE32().arrayFromBuffer(new float[]{0.0f, 1.1f, 2.2f});
		assertSame(PrimSpec.iEEE32(), array.spec());
		assertEquals(3, array.count());
		assertEquals(0.0, ((IEEE32Value)array.getValue(0)).asIEEE32(), DIFF);		
		assertEquals(1.1, ((IEEE32Value)array.getValue(1)).asIEEE32(), DIFF);		
		assertEquals(2.2, ((IEEE32Value)array.getValue(2)).asIEEE32(), DIFF);		

		array = PrimSpec.iEEE64().arrayFromBuffer(new double[]{0.0, 1.1, 2.2});
		assertSame(PrimSpec.iEEE64(), array.spec());
		assertEquals(3, array.count());
		assertEquals(0.0, ((PrimFloatValue)array.getValue(0)).asIEEE64(), DIFF);		
		assertEquals(1.1, ((PrimFloatValue)array.getValue(1)).asIEEE64(), DIFF);		
		assertEquals(2.2, ((PrimFloatValue)array.getValue(2)).asIEEE64(), DIFF);		
		
	}
}
