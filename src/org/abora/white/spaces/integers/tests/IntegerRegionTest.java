/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.spaces.integers.tests;

import junit.framework.TestCase;

import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.value.IntegerValue;

public class IntegerRegionTest extends TestCase {

	public IntegerRegionTest(String arg0) {
		super(arg0);
	}

	public void testAbove() {
		// Exclusive
		IntegerRegion region = IntegerRegion.above(IntegerValue.make(4), false);
		assertFalse(region.hasMember(IntegerPos.make(3)));
		assertFalse(region.hasMember(IntegerPos.make(4)));
		assertTrue(region.hasMember(IntegerPos.make(5)));
		assertTrue(region.hasMember(IntegerPos.make(6)));
		assertFalse(region.isBoundedAbove());

		// Inclusive
		region = IntegerRegion.above(IntegerValue.make(4), true);
		assertFalse(region.hasMember(IntegerPos.make(3)));
		assertTrue(region.hasMember(IntegerPos.make(4)));
		assertTrue(region.hasMember(IntegerPos.make(5)));
		assertTrue(region.hasMember(IntegerPos.make(6)));
		assertFalse(region.isBoundedAbove());
	}

	public void testAfter() {
		IntegerRegion region = IntegerRegion.after(IntegerValue.make(4));
		assertFalse(region.hasMember(IntegerPos.make(3)));
		assertTrue(region.hasMember(IntegerPos.make(4)));
		assertTrue(region.hasMember(IntegerPos.make(5)));
		assertTrue(region.hasMember(IntegerPos.make(6)));
		assertFalse(region.isBoundedAbove());
		
		// Again (optimization)
		region = IntegerRegion.after(IntegerValue.make(4));
		assertFalse(region.hasMember(IntegerPos.make(3)));
		assertTrue(region.hasMember(IntegerPos.make(4)));
		assertTrue(region.hasMember(IntegerPos.make(5)));
		assertTrue(region.hasMember(IntegerPos.make(6)));
		assertFalse(region.isBoundedAbove());
	}
	
	public void testAllIntegers() {
		IntegerRegion region = IntegerRegion.allIntegers();
		assertFalse(region.isBoundedAbove());
		assertFalse(region.isBoundedBelow());
		assertTrue(region.hasMember(IntegerPos.make(-1)));
		assertTrue(region.hasMember(IntegerPos.make(0)));
		assertTrue(region.hasMember(IntegerPos.make(1)));
	}
	
	public void testBefore() {
		IntegerRegion region = IntegerRegion.before(IntegerValue.make(4));
		assertTrue(region.hasMember(IntegerPos.make(2)));
		assertTrue(region.hasMember(IntegerPos.make(3)));
		assertFalse(region.hasMember(IntegerPos.make(4)));
		assertFalse(region.hasMember(IntegerPos.make(5)));
		assertFalse(region.isBoundedBelow());
		
		// Again (optimization)
		region = IntegerRegion.before(IntegerValue.make(4));
		assertTrue(region.hasMember(IntegerPos.make(2)));
		assertTrue(region.hasMember(IntegerPos.make(3)));
		assertFalse(region.hasMember(IntegerPos.make(4)));
		assertFalse(region.hasMember(IntegerPos.make(5)));
		assertFalse(region.isBoundedBelow());
	}

	public void testBelow() {
		IntegerRegion region = IntegerRegion.below(IntegerValue.make(4), false);
		assertTrue(region.hasMember(IntegerPos.make(2)));
		assertTrue(region.hasMember(IntegerPos.make(3)));
		assertFalse(region.hasMember(IntegerPos.make(4)));
		assertFalse(region.hasMember(IntegerPos.make(5)));
		assertFalse(region.isBoundedBelow());
		
		// Again (optimization)
		region = IntegerRegion.below(IntegerValue.make(4), true);
		assertTrue(region.hasMember(IntegerPos.make(2)));
		assertTrue(region.hasMember(IntegerPos.make(3)));
		assertTrue(region.hasMember(IntegerPos.make(4)));
		assertFalse(region.hasMember(IntegerPos.make(5)));
		assertFalse(region.isBoundedBelow());
	}
	
	public void testIntegerExtent() {
		// Empty
		IntegerRegion region = IntegerRegion.integerExtent(IntegerValue.zero(), IntegerValue.zero());
		assertTrue(region.isEmpty());

		region = IntegerRegion.integerExtent(IntegerValue.make(-1), IntegerValue.make(-2));
		assertTrue(region.isEmpty());

		region = IntegerRegion.integerExtent(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.one(), region.count());
		assertFalse(region.hasIntMember(IntegerValue.make(-1)));
		assertTrue(region.hasIntMember(IntegerValue.zero()));
		assertFalse(region.hasIntMember(IntegerValue.one()));

		region = IntegerRegion.integerExtent(IntegerValue.one(), IntegerValue.make(3));
		assertEquals(IntegerValue.make(3), region.count());
		assertFalse(region.hasIntMember(IntegerValue.zero()));
		assertTrue(region.hasIntMember(IntegerValue.one()));
		assertTrue(region.hasIntMember(IntegerValue.make(2)));
		assertTrue(region.hasIntMember(IntegerValue.make(3)));
		assertFalse(region.hasIntMember(IntegerValue.make(4)));
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
