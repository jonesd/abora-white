/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.spaces.integers.tests;

import junit.framework.TestCase;

import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.value.IntegerValue;

public class IntegerRegionTest extends TestCase {

	public IntegerRegionTest(String arg0) {
		super(arg0);
	}
	
	public void testMake() {
		IntegerRegion  region = IntegerRegion.make();
		assertEquals(IntegerValue.zero(), region.count());
	}
	
	public void testInterval() {
		// Empty
		IntegerRegion region = IntegerRegion.interval(IntegerValue.zero(), IntegerValue.zero());
		assertTrue(region.isEmpty());

		region = IntegerRegion.interval(IntegerValue.make(-1), IntegerValue.make(-2));
		assertTrue(region.isEmpty());

		region = IntegerRegion.interval(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.one(), region.count());
		assertFalse(region.hasIntMember(IntegerValue.make(-1)));
		assertTrue(region.hasIntMember(IntegerValue.zero()));
		assertFalse(region.hasIntMember(IntegerValue.one()));

		region = IntegerRegion.interval(IntegerValue.one(), IntegerValue.make(3));
		assertEquals(IntegerValue.make(2), region.count());
		assertFalse(region.hasIntMember(IntegerValue.zero()));
		assertTrue(region.hasIntMember(IntegerValue.one()));
		assertTrue(region.hasIntMember(IntegerValue.make(2)));
		assertFalse(region.hasIntMember(IntegerValue.make(3)));
	}

}
