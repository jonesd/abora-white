/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.spaces.integers.tests;

import junit.framework.TestCase;

import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.spaces.integers.IntegerSpace;
import org.abora.white.tumbler.RealPos;
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

	public void testAsSimpleRegion() {
		// Simple Region
		IntegerRegion region = IntegerRegion.make();
		IntegerRegion simpleRegion = (IntegerRegion)region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertSame(region, simpleRegion);
		
		region = IntegerRegion.make(IntegerValue.one());
		simpleRegion = (IntegerRegion)region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertSame(region, simpleRegion);

		region = IntegerRegion.above(IntegerValue.one(), true);
		simpleRegion = (IntegerRegion)region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertSame(region, simpleRegion);
		
		// Bounded above and below
		region = IntegerRegion.interval(IntegerValue.one(), IntegerValue.make(4));
		region = (IntegerRegion)region.without(IntegerPos.make(2));
		simpleRegion = (IntegerRegion) region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertEquals(IntegerValue.one(), simpleRegion.start());
		assertEquals(IntegerValue.make(4), simpleRegion.stop());
		
		// Bounded below
		region = IntegerRegion.above(IntegerValue.one(), true);
		region = (IntegerRegion)region.without(IntegerPos.make(10));
		simpleRegion = (IntegerRegion) region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertEquals(IntegerValue.one(), simpleRegion.start());
		assertFalse(simpleRegion.isBoundedAbove());

		// Bounded above
		region = IntegerRegion.below(IntegerValue.one(), true);
		region = (IntegerRegion)region.without(IntegerPos.make(-10));
		simpleRegion = (IntegerRegion) region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertEquals(IntegerValue.make(2), simpleRegion.stop());
		assertFalse(simpleRegion.isBoundedBelow());
		
		// Not bounded
		region = IntegerRegion.allIntegers();
		region = (IntegerRegion)region.without(IntegerPos.make(-10));
		simpleRegion = (IntegerRegion) region.asSimpleRegion();
		assertTrue(simpleRegion.isSimple());
		assertFalse(simpleRegion.isBoundedBelow());
		assertFalse(simpleRegion.isBoundedAbove());
	}
	
	public void testBeforeLast() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		IntegerRegion beforeLast = (IntegerRegion)region.beforeLast();
		assertSame(region, beforeLast);

		// Full
		region = IntegerRegion.allIntegers();
		beforeLast = (IntegerRegion)region.beforeLast();
		assertSame(region, beforeLast);

		// Bounded above and below
		region = IntegerRegion.interval(IntegerValue.one(), IntegerValue.make(4));
		region = (IntegerRegion)region.without(IntegerPos.make(2));
		beforeLast = (IntegerRegion) region.beforeLast();
		assertTrue(beforeLast.isSimple());
		assertFalse(beforeLast.isBoundedBelow());
		assertEquals(IntegerValue.make(4), beforeLast.stop());
		
		// Bounded Above
		region = IntegerRegion.before(IntegerValue.make(10));
		region = (IntegerRegion)region.without(IntegerPos.make(3));
		beforeLast = (IntegerRegion)region.beforeLast();
		assertTrue(beforeLast.isSimple());
		assertFalse(beforeLast.isBoundedBelow());
		assertEquals(IntegerValue.make(10), beforeLast.stop());
		
		// Bounded below
		region = IntegerRegion.above(IntegerValue.one(), true);
		region = (IntegerRegion)region.without(IntegerPos.make(10));
		beforeLast = (IntegerRegion) region.beforeLast();
		assertTrue(beforeLast.isSimple());
		assertFalse(beforeLast.isBoundedBelow());
		assertFalse(beforeLast.isBoundedAbove());
	}
	
	public void testCompacted() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		IntegerRegion compacted = (IntegerRegion)region.compacted();
		assertSame(region, compacted);

		// Full
		region = IntegerRegion.allIntegers();
		compacted = (IntegerRegion)region.compacted();
		assertSame(region, compacted);

		// Bounded above and below
		region = IntegerRegion.interval(IntegerValue.make(-4), IntegerValue.make(4));
		region = (IntegerRegion)region.without(IntegerPos.make(2));
		compacted = (IntegerRegion) region.compacted();
		assertTrue(compacted.isSimple());
		assertEquals(IntegerValue.zero(), compacted.start());
		//TODO why isn't following 3? all positions from 0 and on that intersect
		assertEquals(IntegerValue.make(7), compacted.stop());
		
		// Bounded Above
		region = IntegerRegion.before(IntegerValue.make(10));
		region = (IntegerRegion)region.without(IntegerPos.make(3));
		compacted = (IntegerRegion)region.compacted();
		assertTrue(compacted.isSimple());
		assertFalse(compacted.isBoundedBelow());
		assertEquals(IntegerValue.make(9), compacted.stop());
		
		// Bounded below
		region = IntegerRegion.above(IntegerValue.make(-4), true);
		region = (IntegerRegion)region.without(IntegerPos.make(10));
		compacted = (IntegerRegion) region.compacted();
		assertTrue(compacted.isSimple());
		assertEquals(IntegerValue.zero(), compacted.start());
		assertFalse(compacted.isBoundedAbove());
	}

//TODO implement once I understand it	
//	public void testCompactor() {
//	}

	public void testCoordinateSpace() {
		assertTrue(IntegerRegion.make().coordinateSpace() instanceof IntegerSpace);
	}
	
	public void testIsCompacted() {
		// Empty
		assertTrue(IntegerRegion.make().isCompacted());
		
		// Full
		assertFalse(IntegerRegion.allIntegers().isCompacted());
		
		// Bounded above
		assertTrue(IntegerRegion.before(IntegerValue.make(10)).isCompacted());
		
		// Bounded below
		//TODO why isn't following ok? you can generate this response from compact()
		assertFalse(IntegerRegion.after(IntegerValue.zero()).isCompacted());
		assertFalse(IntegerRegion.after(IntegerValue.one()).isCompacted());
		
		// Bounded above and below
		IntegerRegion region = IntegerRegion.interval(IntegerValue.zero(), IntegerValue.make(4)); 
		assertTrue(region.isCompacted());

		region = IntegerRegion.interval(IntegerValue.one(), IntegerValue.make(4)); 		
		assertFalse(region.isCompacted());

		region = IntegerRegion.interval(IntegerValue.zero(), IntegerValue.make(4)); 
		region = (IntegerRegion)region.without(IntegerPos.make(2));
		assertFalse(region.isCompacted());
	}
	
	public void testNearestIntHole() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		assertEquals(IntegerValue.one(), region.nearestIntHole(IntegerValue.one()));
		
		// Full
		region = IntegerRegion.allIntegers();
		try {
			region.nearestIntHole(IntegerValue.one());
			fail("full");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NO_HOLE, e.getMessage());
		}
		
		// Bounded above
		region = IntegerRegion.before(IntegerValue.make(3));
		assertEquals(IntegerValue.make(3), region.nearestIntHole(IntegerValue.one()));
		assertEquals(IntegerValue.make(3), region.nearestIntHole(IntegerValue.make(3)));
		
		// Bounded above (non-simple)		
		region = (IntegerRegion)region.without(IntegerPos.make(-2));
		assertEquals(IntegerValue.make(-2), region.nearestIntHole(IntegerValue.make(-3)));
		assertEquals(IntegerValue.make(-2), region.nearestIntHole(IntegerValue.make(-2)));
		assertEquals(IntegerValue.make(3), region.nearestIntHole(IntegerValue.make(-1)));
		
		// Bounded below
		region = IntegerRegion.after(IntegerValue.make(2));
		assertEquals(IntegerValue.one(), region.nearestIntHole(IntegerValue.one()));
		try {
			region.nearestIntHole(IntegerValue.make(2));
			fail("2");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NO_HOLE, e.getMessage());
		}
		try {
			region.nearestIntHole(IntegerValue.make(3));
			fail("3");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NO_HOLE, e.getMessage());
		}
	}
	
	public void testRunAt() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		IntegerRegion run = region.runAt(IntegerValue.one());
		assertEquals(IntegerRegion.after(IntegerValue.one()), run);
		
		// Full
		region = IntegerRegion.allIntegers();
		run = region.runAt(IntegerValue.one());
		assertEquals(IntegerRegion.after(IntegerValue.one()), run);

		// Bounded above
		region = IntegerRegion.before(IntegerValue.make(3));
		run = region.runAt(IntegerValue.one());
		assertEquals(IntegerRegion.interval(IntegerValue.one(), IntegerValue.make(3)), run);
		run = region.runAt(IntegerValue.make(3));
		assertEquals(IntegerRegion.after(IntegerValue.make(3)), run);

		// Bounded above (non-simple)		
		region = (IntegerRegion)region.without(IntegerPos.make(-2));
		run = region.runAt(IntegerValue.make(-3));
		assertEquals(IntegerRegion.interval(IntegerValue.make(-3), IntegerValue.make(-2)), run);
		run = region.runAt(IntegerValue.make(-2));
		assertEquals(IntegerRegion.interval(IntegerValue.make(-2), IntegerValue.make(-1)), run);
		run = region.runAt(IntegerValue.make(-1));
		assertEquals(IntegerRegion.interval(IntegerValue.make(-1), IntegerValue.make(3)), run);
		
		// Bounded below
		region = IntegerRegion.after(IntegerValue.make(2));
		run = region.runAt(IntegerValue.one());
		assertEquals(IntegerRegion.interval(IntegerValue.one(), IntegerValue.make(2)), run);
		run = region.runAt(IntegerValue.make(2));
		assertEquals(IntegerRegion.after(IntegerValue.make(2)), run);
		run = region.runAt(IntegerValue.make(3));
		assertEquals(IntegerRegion.after(IntegerValue.make(3)), run);
	}
	
	public void testStart() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		try {
			region.start();
			fail("empty");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}
				
		// Full
		region = IntegerRegion.allIntegers();
		try {
			region.start();
			fail("full");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}
		
		// After
		region = IntegerRegion.after(IntegerValue.one());
		assertEquals(IntegerValue.one(), region.start());
		
		// Bonded above and below
		region = IntegerRegion.interval(IntegerValue.make(-4), IntegerValue.make(10));
		assertEquals(IntegerValue.make(-4), region.start());
		
		region = (IntegerRegion)region.without(IntegerPos.make(-1));
		assertEquals(IntegerValue.make(-4), region.start());
	}

	public void testStop() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		try {
			region.stop();
			fail("empty");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}
				
		// Full
		region = IntegerRegion.allIntegers();
		try {
			region.stop();
			fail("full");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}
		
		// Before
		region = IntegerRegion.before(IntegerValue.one());
		assertEquals(IntegerValue.one(), region.stop());
		
		// Bonded above and below
		region = IntegerRegion.interval(IntegerValue.make(-4), IntegerValue.make(10));
		assertEquals(IntegerValue.make(10), region.stop());
		
		region = (IntegerRegion)region.without(IntegerPos.make(-1));
		assertEquals(IntegerValue.make(10), region.stop());
	}
	
	public void testPrintOn() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		assertEquals("{}", region.toString());
				
		// Full
		region = IntegerRegion.allIntegers();
		assertEquals("(-inf, +inf)", region.toString());
		
		// Before
		region = IntegerRegion.before(IntegerValue.one());
		assertEquals("(-inf, 1)", region.toString());
		
		// After
		region = IntegerRegion.after(IntegerValue.one());
		assertEquals("[1, +inf)", region.toString());		
		
		// Bonded above and below
		region = IntegerRegion.interval(IntegerValue.make(-4), IntegerValue.make(10));
		assertEquals("[-4, 10)", region.toString());
		
		region = (IntegerRegion)region.without(IntegerPos.make(-1));
		assertEquals("{[-4, -1), [0, 10)}", region.toString());
		
		region = (IntegerRegion)(IntegerRegion.make(IntegerValue.make(3), IntegerValue.make(7)).unionWith(IntegerRegion.after(IntegerValue.make(10))));
		assertEquals("{[3, 7), [10, +inf)}", region.toString());
	}
	
	public void testActualHashForEqual() {
		IntegerRegion emptyRegion = IntegerRegion.make();
		IntegerRegion fullRegion = IntegerRegion.allIntegers();
		IntegerRegion beforeRegion = IntegerRegion.before(IntegerValue.one());
		IntegerRegion afterRegion = IntegerRegion.after(IntegerValue.one());
		IntegerRegion boundedRegion = IntegerRegion.interval(IntegerValue.make(-4), IntegerValue.make(10));
		IntegerRegion boundedRegion2 = (IntegerRegion)boundedRegion.without(IntegerPos.make(-1));
		IntegerRegion boundedRegion3 = (IntegerRegion)(IntegerRegion.make(IntegerValue.make(3), IntegerValue.make(7)).unionWith(IntegerRegion.after(IntegerValue.make(10))));
		
		assertTrue(emptyRegion.actualHashForEqual() != 0);
		assertTrue(emptyRegion.actualHashForEqual() != fullRegion.actualHashForEqual());
		assertTrue(beforeRegion.actualHashForEqual() != afterRegion.actualHashForEqual());
		assertTrue(boundedRegion.actualHashForEqual() != boundedRegion2.actualHashForEqual());
		assertTrue(boundedRegion2.actualHashForEqual() != boundedRegion3.actualHashForEqual());		
	}
	
	public void testHasIntMember() {
		// Empty
		IntegerRegion region = IntegerRegion.make();
		assertFalse(region.hasIntMember(IntegerValue.one()));
				
		// Full
		region = IntegerRegion.allIntegers();
		assertTrue(region.hasIntMember(IntegerValue.one()));
		
		// Before
		region = IntegerRegion.before(IntegerValue.one());
		assertTrue(region.hasIntMember(IntegerValue.zero()));
		assertFalse(region.hasIntMember(IntegerValue.one()));
		
		// After
		region = IntegerRegion.after(IntegerValue.one());
		assertFalse(region.hasIntMember(IntegerValue.zero()));
		assertTrue(region.hasIntMember(IntegerValue.one()));
		assertTrue(region.hasIntMember(IntegerValue.make(2)));
		
		// Bonded above and below
		region = IntegerRegion.interval(IntegerValue.make(-4), IntegerValue.make(10));
		region = (IntegerRegion)region.without(IntegerPos.make(-1));
		assertFalse(region.hasIntMember(IntegerValue.make(-5)));
		assertTrue(region.hasIntMember(IntegerValue.make(-4)));
		assertTrue(region.hasIntMember(IntegerValue.make(-2)));
		assertFalse(region.hasIntMember(IntegerValue.make(-1)));
		assertTrue(region.hasIntMember(IntegerValue.make(0)));
		assertTrue(region.hasIntMember(IntegerValue.make(9)));
		assertFalse(region.hasIntMember(IntegerValue.make(10)));
				
		region = (IntegerRegion)(IntegerRegion.make(IntegerValue.make(3), IntegerValue.make(7)).unionWith(IntegerRegion.after(IntegerValue.make(10))));
		assertFalse(region.hasIntMember(IntegerValue.make(2)));
		assertTrue(region.hasIntMember(IntegerValue.make(3)));
		assertTrue(region.hasIntMember(IntegerValue.make(6)));
		assertFalse(region.hasIntMember(IntegerValue.make(7)));
		assertFalse(region.hasIntMember(IntegerValue.make(8)));
		assertFalse(region.hasIntMember(IntegerValue.make(9)));
		assertTrue(region.hasIntMember(IntegerValue.make(10)));
		assertTrue(region.hasIntMember(IntegerValue.make(11)));
	}
	
	public void testHasMember() {
		// Empty
		assertFalse(IntegerRegion.make().hasMember(IntegerPos.make(3)));
		
		// Full
		assertTrue(IntegerRegion.allIntegers().hasMember(IntegerPos.make(3)));
		
		// Wrong kind of pos
		try {
			IntegerRegion.allIntegers().hasMember(RealPos.make(1.0));
			fail("realpos");
		} catch (ClassCastException e) {
			// expected
		}
	}
}
