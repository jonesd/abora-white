/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.spaces.basic.tests;

import junit.framework.TestCase;

import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.spaces.basic.IntegerUpOrder;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.spaces.integers.IntegerSpace;
import org.abora.white.tumbler.RealPos;
import org.abora.white.tumbler.RealUpOrder;
import org.abora.white.value.IntegerValue;

public class IntegerUpOrderTest extends TestCase {

	public IntegerUpOrderTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();
		assertNotNull(order);
		//TODO more
	}
	
	public void testActualHashForEqual() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();
		assertTrue(order.actualHashForEqual() != 0);
		assertTrue(order.actualHashForEqual() == IntegerUpOrder.make().actualHashForEqual());		
	}

	public void testFollows() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();
		assertTrue(order.follows(IntegerPos.make(10), IntegerPos.make(10)));		
		assertTrue(order.follows(IntegerPos.make(11), IntegerPos.make(10)));		
		assertFalse(order.follows(IntegerPos.make(9), IntegerPos.make(10)));
		
		try {
			order.follows(IntegerPos.make(10), RealPos.make(10.0f));
			fail("realpos");		
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testFollowsInt() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();
		assertTrue(order.followsInt(IntegerValue.make(10), IntegerValue.make(10)));		
		assertTrue(order.followsInt(IntegerValue.make(11), IntegerValue.make(10)));		
		assertFalse(order.followsInt(IntegerValue.make(9), IntegerValue.make(10)));
	}

	public void testIsEqual() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();
		
		assertTrue(order.isEqual(IntegerUpOrder.make()));
		
		assertFalse(order.isEqual(IntegerValue.make(10)));		
		assertFalse(order.isEqual(RealUpOrder.make()));		
	}
	
	public void testIsFullOrderWith() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();

		assertTrue(order.isFullOrder(null));
		assertTrue(order.isFullOrder(IntegerRegion.interval(IntegerValue.make(10), IntegerValue.make(15))));		
	}
	
	public void testPreceeds() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();

		try {
			//TODO should this really fail?
			order.preceeds(IntegerRegion.make(), IntegerRegion.make());
			fail("empty");		
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}

		assertTrue(order.preceeds(IntegerRegion.allIntegers(), IntegerRegion.allIntegers()));
		
		assertTrue(order.preceeds(IntegerRegion.before(IntegerValue.make(10)), IntegerRegion.after(IntegerValue.make(2))));
		assertFalse(order.preceeds(IntegerRegion.after(IntegerValue.make(2)), IntegerRegion.before(IntegerValue.make(10))));
		
		assertTrue(order.preceeds(IntegerRegion.make(IntegerValue.make(2)), IntegerRegion.make(IntegerValue.make(2))));
		assertTrue(order.preceeds(IntegerRegion.make(IntegerValue.make(2)), IntegerRegion.make(IntegerValue.make(3))));
		assertFalse(order.preceeds(IntegerRegion.make(IntegerValue.make(3)), IntegerRegion.make(IntegerValue.make(2))));
	}
	
	public void testArrange() {
		//TODO once Arrangements have been tested
	}
	
	public void testChooseMany() {
		//TODO once Arrangements have been tested
	}

	public void testChooseOne() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();

		IntegerRegion region = IntegerRegion.make(IntegerValue.make(5));
		assertEquals(IntegerPos.make(5), order.chooseOne(region));

		region = IntegerRegion.interval(IntegerValue.make(5), IntegerValue.make(10));
		assertEquals(IntegerPos.make(5), order.chooseOne(region));

		try {
			order.chooseOne(IntegerRegion.make());
			fail("empty");		
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}

		try {
			order.chooseOne(IntegerRegion.allIntegers());
			fail("full");		
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.INVALID_REQUEST, e.getMessage());
		}
	}

	public void testCoordinateSpace() {
		IntegerUpOrder order = (IntegerUpOrder)IntegerUpOrder.make();
		//TODO poor test...
		assertTrue(order.coordinateSpace() instanceof IntegerSpace);		
	}
}
