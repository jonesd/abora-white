/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.sets.tests;

import junit.framework.TestCase;

import org.abora.white.collection.sets.ImmuSet;
import org.abora.white.collection.sets.MuSet;
import org.abora.white.collection.sets.SetAccumulator;
import org.abora.white.collection.steppers.Accumulator;
import org.abora.white.value.IntegerValue;

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
