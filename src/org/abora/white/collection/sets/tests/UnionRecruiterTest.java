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
import org.abora.white.collection.sets.UnionRecruiter;
import org.abora.white.collection.steppers.Accumulator;
import org.abora.white.value.IntegerValue;

public class UnionRecruiterTest extends TestCase {

	public UnionRecruiterTest(String arg0) {
		super(arg0);
	}
	
	public void testMake() {
		UnionRecruiter recruiter = UnionRecruiter.make();
		ImmuSet set = (ImmuSet)recruiter.value();
		assertTrue(set.isEmpty());
	}

	public void testStep() {
		UnionRecruiter recruiter = UnionRecruiter.make();
		
		// Step empty
		recruiter.step(ImmuSet.make());
		
		// Step one
		MuSet set = MuSet.make();
		set.introduce(IntegerValue.make(99));
		recruiter.step(set);
		
		// Ensure recruiter not connected to previous set
		set.introduce(IntegerValue.make(100));
		
		// Step multiple with duplicate
		MuSet set2 = MuSet.make();
		set2.introduce(IntegerValue.make(98));
		set2.introduce(IntegerValue.make(99));
		recruiter.step(set2);
		
		// Non-set step
		try {
			recruiter.step(IntegerValue.make(101));
			fail("101");
		} catch (ClassCastException e) {
			// expected
		}
		
		ImmuSet value = (ImmuSet)recruiter.value();
		assertEquals(IntegerValue.make(2), value.count());
		assertTrue(value.hasMember(IntegerValue.make(99)));
		assertTrue(value.hasMember(IntegerValue.make(98)));		
	}

	public void testValue() {
		// Initial copy
		UnionRecruiter recruiter = UnionRecruiter.make();
		ImmuSet value0 = (ImmuSet)recruiter.value();

		// Step		
		MuSet set1 = MuSet.make();
		set1.introduce(IntegerValue.make(99));
		recruiter.step(set1);
		ImmuSet value1 = (ImmuSet)recruiter.value();
		
		// Step both accumulators
		MuSet set2 = MuSet.make();
		set2.introduce(IntegerValue.make(98));
		recruiter.step(set2);
		ImmuSet value2 = (ImmuSet)recruiter.value();

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
		UnionRecruiter recruiter = UnionRecruiter.make();
		
		// Copy after first step
		MuSet set1 = MuSet.make();
		set1.introduce(IntegerValue.make(99));
		recruiter.step(set1);
		Accumulator recruiterCopy = recruiter.copy();
		
		// Step both accumulators
		MuSet set2 = MuSet.make();
		set2.introduce(IntegerValue.make(98));
		recruiter.step(set2);

		MuSet set3 = MuSet.make();
		set3.introduce(IntegerValue.make(100));
		recruiterCopy.step(set3);
				
		// Ensure accumulators have stepped separately
		ImmuSet set = (ImmuSet)recruiter.value();
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));		

		ImmuSet setCopy = (ImmuSet)recruiterCopy.value();
		assertEquals(IntegerValue.make(2), setCopy.count());
		assertTrue(setCopy.hasMember(IntegerValue.make(99)));
		assertTrue(setCopy.hasMember(IntegerValue.make(100)));		
	}
}
