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
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.ReverseOrder;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.spaces.integers.IntegerSpace;
import info.dgjones.abora.white.tumbler.RealPos;
import info.dgjones.abora.white.tumbler.RealUpOrder;
import info.dgjones.abora.white.value.IntegerValue;

public class ReverseOrderTest extends TestCase {

	public ReverseOrderTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);
		assertNotNull(order);
		
		try {
			ReverseOrder.make(null);
			fail("null");
		} catch (IllegalArgumentException e) {
			// expected
		}
	}

	public void testCoordinateSpace() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertTrue(order.coordinateSpace() instanceof IntegerSpace);
		
		// Use root orders coordinate space
		ReverseOrder order2 = (ReverseOrder)ReverseOrder.make(order);
		assertTrue(order2.coordinateSpace() instanceof IntegerSpace);
	}
	
	public void testReversed() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertSame(integerOrder, order.reversed());
	}
	
	public void testActualHashForEqual() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertTrue(order.actualHashForEqual() != 0);
		assertTrue(order.actualHashForEqual() != integerOrder.actualHashForEqual());
		
		OrderSpec realOrder = RealUpOrder.make();
		assertTrue(order.actualHashForEqual() != ReverseOrder.make(realOrder).actualHashForEqual());
	}

	public void testFollows() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertTrue(order.follows(IntegerPos.make(10), IntegerPos.make(10)));		
		assertFalse(order.follows(IntegerPos.make(11), IntegerPos.make(10)));		
		assertTrue(order.follows(IntegerPos.make(9), IntegerPos.make(10)));
		
		try {
			order.follows(IntegerPos.make(10), RealPos.make(10.0f));
			fail("realpos");		
		} catch (ClassCastException e) {
			// expected
		}
	}

	public void testFollowsInt() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertTrue(order.followsInt(IntegerValue.make(10), IntegerValue.make(10)));		
		assertFalse(order.followsInt(IntegerValue.make(11), IntegerValue.make(10)));		
		assertTrue(order.followsInt(IntegerValue.make(9), IntegerValue.make(10)));
	}
	
	public void testIsEqual() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertTrue(order.isEqual(order));
		assertTrue(order.isEqual(ReverseOrder.make(integerOrder)));
		
		assertFalse(order.isEqual(integerOrder));
		assertFalse(order.isEqual(ReverseOrder.make(RealUpOrder.make())));

		assertFalse(order.isEqual(IntegerValue.make(10)));
		assertFalse(order.isEqual(null));		
	}
	
	public void testIsFullOrder() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		assertTrue(order.isFullOrder(null));
		assertTrue(order.isFullOrder(IntegerRegion.interval(IntegerValue.make(10), IntegerValue.make(15))));			
	}
	
	public void testPreceeds() {
		OrderSpec integerOrder = IntegerUpOrder.make();
		ReverseOrder order = (ReverseOrder)ReverseOrder.make(integerOrder);

		try {
			order.preceeds(IntegerRegion.allIntegers(), IntegerRegion.allIntegers());
			fail("unsupported");
		} catch (UnsupportedOperationException e) {
			// expected
		}
	}
}
