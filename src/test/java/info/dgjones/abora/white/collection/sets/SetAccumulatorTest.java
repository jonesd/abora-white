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
package info.dgjones.abora.white.collection.sets;

import junit.framework.TestCase;

import info.dgjones.abora.white.collection.sets.ImmuSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.collection.sets.SetAccumulator;
import info.dgjones.abora.white.collection.steppers.Accumulator;
import info.dgjones.abora.white.value.IntegerValue;

public class SetAccumulatorTest extends TestCase {

	public SetAccumulatorTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		SetAccumulator accumulator = SetAccumulator.make();
		ImmuSet set = (ImmuSet)accumulator.value();
		assertTrue(set.isEmpty());
	}
	
	public void testMakeFromMu() {
		// Empty
		MuSet startSet = MuSet.make();
		SetAccumulator accumulator = SetAccumulator.make(startSet);
		ImmuSet set = (ImmuSet)accumulator.value();
		assertTrue(set.isEmpty());
		
		// Modifying startSet should not effect accumulator results
		startSet.introduce(IntegerValue.make(99));
		assertTrue(set.isEmpty());

		// Test with one element
		accumulator = SetAccumulator.make(startSet);
		set = (ImmuSet)accumulator.value();
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));

		// Modifying startSet should not effect accumulator results
		startSet.introduce(IntegerValue.make(98));
		set = (ImmuSet)accumulator.value();
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
	}

	public void testMakeFromImmu() {
		// Empty
		ImmuSet startSet = ImmuSet.make();
		SetAccumulator accumulator = SetAccumulator.make(startSet);
		ImmuSet set = (ImmuSet)accumulator.value();
		assertTrue(set.isEmpty());
		
		// Modifying startSet should not effect accumulator results
		startSet = startSet.with(IntegerValue.make(99));
		assertTrue(set.isEmpty());
		
		// Test with one element
		accumulator = SetAccumulator.make(startSet);
		set = (ImmuSet)accumulator.value();
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));

		// Modifying startSet should not effect accumulator results
		startSet = startSet.with(IntegerValue.make(98));
		set = (ImmuSet)accumulator.value();
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
	}
	
	public void testStep() {
		SetAccumulator accumulator = SetAccumulator.make();
		accumulator.step(IntegerValue.make(99));
		accumulator.step(IntegerValue.make(98));
		// Duplicates allowed
		accumulator.step(IntegerValue.make(99));
		ImmuSet set = (ImmuSet)accumulator.value();
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));		
	}

	public void testValue() {
		// Initial copy
		SetAccumulator accumulator = SetAccumulator.make();
		ImmuSet value0 = (ImmuSet)accumulator.value();

		// Step		
		accumulator.step(IntegerValue.make(99));
		ImmuSet value1 = (ImmuSet)accumulator.value();
		
		// Step both accumulators
		accumulator.step(IntegerValue.make(98));
		ImmuSet value2 = (ImmuSet)accumulator.value();

		// Ensure values are correct and separate
		assertTrue(value0.isEmpty());
		
		assertEquals(IntegerValue.one(), value1.count());
		assertTrue(value1.hasMember(IntegerValue.make(99)));
		
		assertEquals(IntegerValue.make(2), value2.count());
		assertTrue(value2.hasMember(IntegerValue.make(99)));
		assertTrue(value2.hasMember(IntegerValue.make(98)));		
	}
	
	public void testCopy() {
		// Initial copy
		SetAccumulator accumulator = SetAccumulator.make();
		
		// Copy after first step
		accumulator.step(IntegerValue.make(99));
		Accumulator accumulatorCopy = accumulator.copy();
		
		// Step both accumulators
		accumulator.step(IntegerValue.make(98));
		accumulatorCopy.step(IntegerValue.make(100));
				
		// Ensure accumulators have stepped separately
		ImmuSet set = (ImmuSet)accumulator.value();
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));		

		ImmuSet setCopy = (ImmuSet)accumulatorCopy.value();
		assertEquals(IntegerValue.make(2), setCopy.count());
		assertTrue(setCopy.hasMember(IntegerValue.make(99)));
		assertTrue(setCopy.hasMember(IntegerValue.make(100)));		
	}
}
