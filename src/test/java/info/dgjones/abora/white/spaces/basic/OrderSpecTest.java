/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package info.dgjones.abora.white.spaces.basic;

import junit.framework.TestCase;

import info.dgjones.abora.white.spaces.basic.IntegerUpOrder;
import info.dgjones.abora.white.spaces.basic.OrderEnum;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.ReverseOrder;
import info.dgjones.abora.white.spaces.integers.IntegerPos;

public class OrderSpecTest extends TestCase {

	public OrderSpecTest(String arg0) {
		super(arg0);
	}

	public void testCompare() {
		OrderSpec order = IntegerUpOrder.make();
		ReverseOrder reverseOrder = (ReverseOrder)ReverseOrder.make(order);

		assertEquals(OrderEnum.EQUAL, order.compare(IntegerPos.make(10), IntegerPos.make(10)));
		assertEquals(OrderEnum.GREATER, order.compare(IntegerPos.make(11), IntegerPos.make(10)));
		assertEquals(OrderEnum.LESS, order.compare(IntegerPos.make(10), IntegerPos.make(11)));
		//TODO test incomparable
		
		assertEquals(OrderEnum.EQUAL, reverseOrder.compare(IntegerPos.make(10), IntegerPos.make(10)));
		assertEquals(OrderEnum.LESS, reverseOrder.compare(IntegerPos.make(11), IntegerPos.make(10)));
		assertEquals(OrderEnum.GREATER, reverseOrder.compare(IntegerPos.make(10), IntegerPos.make(11)));
		//TODO test incomparable
	}
	
	public void testIsFullOrder() {
		OrderSpec order = IntegerUpOrder.make();

		assertTrue(order.isFullOrder());
	}
	
	public void testReversed() {
		OrderSpec order = IntegerUpOrder.make();
		
		ReverseOrder reverseOrder = (ReverseOrder)order.reversed();
		assertEquals(order, reverseOrder.reversed());
	}
}
