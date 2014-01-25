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
