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
package info.dgjones.abora.white.value;

import junit.framework.TestCase;

import info.dgjones.abora.white.value.PrimFloatValue;
import info.dgjones.abora.white.value.PrimSpec;

public class PrimFloatSpecTest extends TestCase {
	public PrimFloatSpecTest(String arg0) {
		super(arg0);
	}

	public void testValue() {
		PrimFloatValue value = PrimSpec.iEEE32().value(123.456);
		assertEquals(123.456f, value.asIEEE32(), 0.00001f);

		value = PrimSpec.iEEE64().value(123.456);
		assertEquals(123.456f, value.asIEEE64(), 0.00001f);
	}

	public void testIsEqual() {
		assertTrue(PrimSpec.iEEE32().isEqual(PrimSpec.iEEE32()));
		assertFalse(PrimSpec.iEEE32().isEqual(PrimSpec.iEEE64()));
		assertFalse(PrimSpec.iEEE32().isEqual(PrimSpec.int32()));		
	}
	
	public void testActualHashForEqual() {
		assertTrue(PrimSpec.iEEE32().actualHashForEqual() == PrimSpec.iEEE32().actualHashForEqual());
		assertFalse(PrimSpec.iEEE32().actualHashForEqual() == PrimSpec.iEEE64().actualHashForEqual());
		assertFalse(PrimSpec.iEEE32().actualHashForEqual() == PrimSpec.int32().actualHashForEqual());
	}
}
