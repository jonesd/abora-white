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
import org.abora.white.collection.sets.ImmuSetOnMu;
import org.abora.white.collection.sets.MuSet;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class ImmuSetOnMuTest extends TestCase {

	public ImmuSetOnMuTest(String arg0) {
		super(arg0);
	}

	protected ImmuSetOnMu getImmuSetOnMu() {
		MuSet muSet = MuSet.make();
		muSet.introduce(IntegerValue.make(99));
		muSet.introduce(IntegerValue.make(98));
		return (ImmuSetOnMu)muSet.asImmuSet();
	}

	public void testHasMember() {
		ImmuSet set = getImmuSetOnMu();
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		assertFalse(set.hasMember(IntegerValue.make(100)));
	}
	
	public void testIsEmpty() {
		ImmuSet set = getImmuSetOnMu();
		assertFalse(set.isEmpty());		
	}

	public void testCount() {
		ImmuSet set = getImmuSetOnMu();
		assertEquals(IntegerValue.make(2), set.count());
	}
	
	public void testIsSubsetOf() {
		ImmuSet set1 = getImmuSetOnMu();
		ImmuSet set2 = getImmuSetOnMu();
		
		// Subset
		assertTrue(set1.isSubsetOf(set2));
		assertTrue(set2.isSubsetOf(set1));
		
		// Not-subset
		set1 = set1.with(IntegerValue.make(97));
		assertFalse(set1.isSubsetOf(set2));
		assertTrue(set2.isSubsetOf(set1));
		
		// Empty
		assertFalse(set1.isSubsetOf(ImmuSet.make()));
	}
	
	public void testStepper() {
		ImmuSet set = getImmuSetOnMu();
		Stepper stepper = set.stepper();
		Heaper heaper1 = stepper.get();
		stepper.step();
		Heaper heaper2 = stepper.get();
		stepper.step();
		assertTrue(stepper.atEnd());
		assertTrue(heaper1.isEqual(IntegerValue.make(99)) || heaper1.isEqual(IntegerValue.make(98)));
		assertTrue(heaper2.isEqual(IntegerValue.make(99)) || heaper2.isEqual(IntegerValue.make(98)));
	}
	
	public void testTheOne() {
		ImmuSet set = getImmuSetOnMu();
		try {
			set.theOne();
			fail("theOne");		
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_ONE_ELEMENT, e.getMessage());
		}
	}
	
	public void testIntersect() {
		ImmuSet set = getImmuSetOnMu();
		
		// Empty
		ImmuSet intersectSet = set.intersect(ImmuSet.make());
		assertTrue(intersectSet.isEmpty());
		
		// Not-empty
		MuSet otherSet = MuSet.make();
		otherSet.introduce(IntegerValue.make(99));
		otherSet.introduce(IntegerValue.make(100));
		intersectSet = set.intersect(otherSet);
		assertEquals(IntegerValue.one(), intersectSet.count());
		assertTrue(intersectSet.hasMember(IntegerValue.make(99)));
	}
	
	public void testMinus() {
		ImmuSet set = getImmuSetOnMu();
		
		// Empty
		ImmuSet minusSet = set.minus(ImmuSet.make());
		assertSame(set, minusSet);

		// Not-empty
		MuSet otherSet = MuSet.make();
		otherSet.introduce(IntegerValue.make(99));
		otherSet.introduce(IntegerValue.make(100));
		minusSet = set.minus(otherSet);
		assertEquals(IntegerValue.one(), minusSet.count());
		assertTrue(minusSet.hasMember(IntegerValue.make(98)));
	}
	
	public void testUnionWith() {
		ImmuSet set = getImmuSetOnMu();
		
		// Empty
		ImmuSet unionSet = set.unionWith(ImmuSet.make());
		assertSame(set, unionSet);

		// Not-empty
		MuSet otherSet = MuSet.make();
		otherSet.introduce(IntegerValue.make(99));
		otherSet.introduce(IntegerValue.make(97));
		unionSet = set.unionWith(otherSet);
		assertEquals(IntegerValue.make(3), unionSet.count());
		assertTrue(unionSet.hasMember(IntegerValue.make(99)));
		assertTrue(unionSet.hasMember(IntegerValue.make(98)));
		assertTrue(unionSet.hasMember(IntegerValue.make(97)));
		
		// Receiver smaller
		unionSet = set.unionWith(unionSet);
		assertEquals(IntegerValue.make(3), unionSet.count());
		assertTrue(unionSet.hasMember(IntegerValue.make(99)));
		assertTrue(unionSet.hasMember(IntegerValue.make(98)));
		assertTrue(unionSet.hasMember(IntegerValue.make(97)));
	}
	
	public void testWith() {
		ImmuSet set = getImmuSetOnMu();

		ImmuSet withSet = set.with(IntegerValue.make(97));
		assertEquals(IntegerValue.make(3), withSet.count());
		assertTrue(withSet.hasMember(IntegerValue.make(99)));
		assertTrue(withSet.hasMember(IntegerValue.make(98)));
		assertTrue(withSet.hasMember(IntegerValue.make(97)));		
	}
	
	public void testWithout() {
		ImmuSet set = getImmuSetOnMu();

		ImmuSet withoutSet = set.without(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), withoutSet.count());
		assertTrue(withoutSet.hasMember(IntegerValue.make(98)));
	}
	
	public void testAsMuSet() {
		ImmuSet set = getImmuSetOnMu();
	
		MuSet muSet = set.asMuSet();
		assertEquals(IntegerValue.make(2), muSet.count());
		assertTrue(muSet.hasMember(IntegerValue.make(99)));
		assertTrue(muSet.hasMember(IntegerValue.make(98)));
	}
}
