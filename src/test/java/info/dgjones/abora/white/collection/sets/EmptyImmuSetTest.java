/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package info.dgjones.abora.white.collection.sets;

import junit.framework.TestCase;

import info.dgjones.abora.white.collection.sets.EmptyImmuSet;
import info.dgjones.abora.white.collection.sets.ImmuSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.value.IntegerValue;

public class EmptyImmuSetTest extends TestCase {
	public EmptyImmuSet set;

	public EmptyImmuSetTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(EmptyImmuSetTest.class);
	}

	public void setUp() {
		set = (EmptyImmuSet) ImmuSet.make();

	}

	public void testCount() {
		assertEquals(IntegerValue.zero(), set.count());
	}
	
	public void testTheOne() {
		try {
			set.theOne();
			fail();
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_ONE_ELEMENT, e.getMessage());
		}
	}
	
	public void testStepper() {
		Stepper stepper = set.stepper();
		assertTrue(stepper.atEnd());
	}

	public void testWith() {
		ImmuSet withSet = set.with(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), withSet.count());
		assertTrue(withSet.hasMember(IntegerValue.make(99)));
		assertFalse(withSet.hasMember(IntegerValue.make(98)));
	}
	
	public void testWithout() {
		ImmuSet withoutSet = set.without(IntegerValue.make(99));
		assertEquals(IntegerValue.zero(), withoutSet.count());
		assertFalse(withoutSet.hasMember(IntegerValue.make(99)));
	}
	
	public void testHasMember() {
		assertFalse(set.hasMember(IntegerValue.one()));
		assertFalse(set.hasMember(null));
	}
	
	public void testIsEmpty() {
		assertTrue(set.isEmpty());
	}
	public void testIsSubsetOf() {
		assertTrue(set.isSubsetOf(set));
		assertTrue(set.isSubsetOf(ImmuSet.make()));
		assertTrue(set.isSubsetOf(ImmuSet.newWith(IntegerValue.zero())));
		assertTrue(set.isSubsetOf(MuSet.make(IntegerPos.make(IntegerValue.zero()))));	
	}
	
	public void testIntersect() {
		assertTrue(set.intersect(set).isEmpty());
		assertTrue(set.intersect(ImmuSet.newWith(IntegerValue.zero())).isEmpty());
	}

	public void testMinus() {
		assertTrue(set.minus(set).isEmpty());
		assertTrue(set.minus(ImmuSet.newWith(IntegerValue.zero())).isEmpty());
	}
	
	public void testUnionWith() {
		assertTrue(set.unionWith(set).isEmpty());

		ImmuSet unionSet = set.unionWith(ImmuSet.newWith(IntegerValue.zero()));
		assertEquals(IntegerValue.one(), unionSet.count());
		assertTrue(unionSet.hasMember(IntegerValue.zero()));

		unionSet = set.unionWith(MuSet.make(IntegerPos.make(IntegerValue.zero())));
		assertEquals(IntegerValue.one(), unionSet.count());
		assertTrue(unionSet.hasMember(IntegerPos.make(IntegerValue.zero())));
	}
	
	public void testAsMuSet() {
		MuSet muSet = set.asMuSet();
		assertTrue(muSet.isEmpty());
	}
}
