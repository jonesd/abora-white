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
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.value.IntegerValue;

public class MuSetTest extends TestCase {

	public MuSetTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		MuSet set = MuSet.make();
		assertTrue(set.isEmpty());
	}
	
	public void testMakeWith() {
		MuSet set = MuSet.make(IntegerPos.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue (set.hasMember(IntegerPos.make(99)));
	}
	
	public void testMakeCapacity() {
		MuSet set = MuSet.make(IntegerValue.make(23));
		assertTrue(set.isEmpty());
	}

	public void testFromStepper() {
		// Empty
		Stepper stepper = ImmuSet.make().stepper();
		MuSet muSet = MuSet.fromStepper(stepper);
		assertTrue(muSet.isEmpty());
		assertTrue(stepper.atEnd());
		
		// One element
		MuSet stepperSet = MuSet.make();
		stepperSet.introduce(IntegerValue.make(99));
		stepper = stepperSet.stepper();
		muSet = MuSet.fromStepper(stepper);
		assertEquals(IntegerValue.one(), muSet.count());
		assertTrue(muSet.hasMember(IntegerValue.make(99)));
		
		// Two elements
		stepperSet.introduce(IntegerValue.make(98));
		stepper = stepperSet.stepper();
		muSet = MuSet.fromStepper(stepper);
		assertEquals(IntegerValue.make(2), muSet.count());
		assertTrue(muSet.hasMember(IntegerValue.make(99)));
		assertTrue(muSet.hasMember(IntegerValue.make(98)));
		
		//TODO possibly test against TableStepper?		
	}

	public void testRestrictTo() {
		// Empty
		MuSet muSet = MuSet.make();
		MuSet otherSet = MuSet.make();
		muSet.restrictTo(otherSet);
		assertTrue(muSet.isEmpty());
		
		// empty no-intersection
		muSet.introduce(IntegerValue.make(99));
		muSet.introduce(IntegerValue.make(98));
		muSet.restrictTo(otherSet);
		assertTrue(muSet.isEmpty());

		// intersection
		muSet.introduce(IntegerValue.make(99));
		muSet.introduce(IntegerValue.make(98));
		otherSet.introduce(IntegerValue.make(98));
		otherSet.introduce(IntegerValue.make(80));
		otherSet.introduce(IntegerValue.make(81));
		muSet.restrictTo(otherSet);
		assertEquals(IntegerValue.one(), muSet.count());
		assertTrue(muSet.hasMember(IntegerValue.make(98)));

	}

	public void testAsImmuSet() {
		// Empty
		MuSet muSet = MuSet.make();
		ImmuSet immuSet = muSet.asImmuSet();
		assertTrue(immuSet.isEmpty());
		
		// One element
		muSet = MuSet.make();
		muSet.introduce(IntegerValue.make(99));
		immuSet = muSet.asImmuSet();
		assertEquals(IntegerValue.one(), immuSet.count());
		assertTrue(immuSet.hasMember(IntegerValue.make(99)));
		
		// Multiple elements
		muSet = MuSet.make();
		muSet.introduce(IntegerValue.make(99));
		muSet.introduce(IntegerValue.make(98));		
		immuSet = muSet.asImmuSet();
		assertEquals(IntegerValue.make(2), immuSet.count());
		assertTrue(immuSet.hasMember(IntegerValue.make(99)));		
		assertTrue(immuSet.hasMember(IntegerValue.make(98)));		
	}
	
	public void testAsMuSet() {
		// Empty
		MuSet muSet = MuSet.make();
		MuSet asSet = muSet.asMuSet();
		assertNotSame(muSet, asSet);
		assertTrue(asSet.isEmpty());
		
		// One element
		muSet = MuSet.make();
		muSet.introduce(IntegerValue.make(99));
		asSet = muSet.asMuSet();
		assertNotSame(muSet, asSet);
		assertEquals(IntegerValue.one(), asSet.count());
		assertTrue(asSet.hasMember(IntegerValue.make(99)));
		
		// Multiple elements
		muSet = MuSet.make();
		muSet.introduce(IntegerValue.make(99));
		muSet.introduce(IntegerValue.make(98));		
		asSet = muSet.asMuSet();
		assertNotSame(muSet, asSet);
		assertEquals(IntegerValue.make(2), asSet.count());
		assertTrue(asSet.hasMember(IntegerValue.make(99)));		
		assertTrue(asSet.hasMember(IntegerValue.make(98)));		
	}
	
	public void testActualHashForEqual() {
		MuSet muSet1 = MuSet.make();
		MuSet muSet2 = MuSet.make();
		MuSet muSet3 = MuSet.make();
		muSet3.introduce(IntegerValue.make(99));
		
		assertTrue(muSet1.actualHashForEqual() != 0);
		assertTrue(muSet1.actualHashForEqual() == muSet1.actualHashForEqual());
		assertTrue(muSet1.actualHashForEqual() != muSet2.actualHashForEqual());
		assertTrue(muSet1.actualHashForEqual() != muSet3.actualHashForEqual());
	}
	
	public void testIsEqual() {
		MuSet muSet1 = MuSet.make();
		MuSet muSet2 = MuSet.make();
		MuSet muSet3 = MuSet.make();
		muSet3.introduce(IntegerValue.make(99));
		
		assertTrue(muSet1.isEqual(muSet1));
		assertFalse(muSet1.isEqual(muSet2));
		assertFalse(muSet1.isEqual(muSet3));		
	}
}
