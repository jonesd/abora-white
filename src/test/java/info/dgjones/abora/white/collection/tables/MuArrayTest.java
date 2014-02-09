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

/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * 
 */
package info.dgjones.abora.white.collection.tables;

import info.dgjones.abora.white.collection.steppers.TableAccumulator;
import info.dgjones.abora.white.spaces.integers.IntegerMapping;
import info.dgjones.abora.white.value.IntegerValue;
import junit.framework.TestCase;

public class MuArrayTest extends TestCase {

	public MuArrayTest(String arg0) {
		super(arg0);
	}

	public void testArray() {
		MuArray array = MuArray.array();
		assertTrue(array.isEmpty());
	}

	public void testArrayWith() {
		MuArray array = MuArray.array(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
	}

	public void testArrayWithWith() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
	}

	public void testArrayWithWithWith() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97));
		assertEquals(IntegerValue.make(3), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
		assertEquals(IntegerValue.make(97), array.intGet(IntegerValue.make(2)));
	}

	public void testArrayWithWithWithWith() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
		assertEquals(IntegerValue.make(4), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
		assertEquals(IntegerValue.make(97), array.intGet(IntegerValue.make(2)));
		assertEquals(IntegerValue.make(96), array.intGet(IntegerValue.make(3)));
	}
	
	public void testArrayAccumulator() {
		// Empty
		TableAccumulator accumulator = MuArray.arrayAccumulator();
		assertTrue(((MuArray)accumulator.value()).isEmpty());
		
		// After stepping
		accumulator.step(IntegerValue.make(99));
		accumulator.step(IntegerValue.make(98));
		MuArray array = (MuArray)accumulator.value();
		assertEquals(IntegerValue.make(2), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
	}
	
	public void testArrayAccumulatorWith() {
		MuArray array = MuArray.array(IntegerValue.make(99));
		
		TableAccumulator accumulator = MuArray.arrayAccumulator(array);
		accumulator.step(IntegerValue.make(98));

		assertEquals(IntegerValue.make(2), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));		
	}
	
	public void testMake() {
		MuArray array = (MuArray)MuArray.make(IntegerValue.make(10));
		assertTrue(array.isEmpty());
		//TODO do better in finding actual capacity
	}
	
	public void testOffsetScruArray() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
		ScruTable offsetTable = MuArray.offsetScruArray(array, IntegerMapping.make(IntegerValue.make(-2)));
		assertEquals(IntegerValue.make(97), offsetTable.intGet(IntegerValue.zero()));
	}
	
//TODO
//	public void testOffsetSubTableBetween() {
//		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
//		ScruTable subtable = array.offsetSubTableBetween(IntegerValue.one(), IntegerValue.make(3), IntegerValue.make(0));
//		assertEquals(IntegerValue.make(2), subtable.count());
//		assertEquals(IntegerValue.make(98), subtable.intGet(IntegerValue.zero()));
//		assertEquals(IntegerValue.make(97), subtable.intGet(IntegerValue.one()));		
//	}

//	public void testTransformedBy() {
//		// Identity
//		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
//		Dsp dsp = IntegerMapping.make();
//		ScruTable table = array.transformedBy(dsp);
//		assertSame(array, table);
//		
//		// Simple
//		dsp = IntegerMapping.make(IntegerValue.one());
//		table = array.transformedBy(dsp);
//		assertEquals(IntegerValue.make(98), table.intGet(IntegerValue.zero()));
//		
//	}
}
