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
import org.abora.white.collection.sets.TinyImmuSet;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.value.IntegerValue;

public class TinyImmuSetTest extends TestCase {

	public TinyImmuSetTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		TinyImmuSet set = (TinyImmuSet)TinyImmuSet.make(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
	}
	
	public void testCount() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
	}
	
	public void testTheOne() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		assertEquals(IntegerValue.make(99), set.theOne());
	}

	public void testStepper() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		Stepper stepper = set.stepper();
		assertFalse(stepper.atEnd());
		assertEquals(IntegerValue.make(99), stepper.get());
		stepper.step();
		assertTrue(stepper.atEnd());
	}

	public void testWith() {
		// Same element
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		ImmuSet withSet = set.with(IntegerValue.make(99));
		assertSame(set, withSet);
		
		// Different element
		withSet = set.with(IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), withSet.count());
		assertTrue(withSet.hasMember(IntegerValue.make(99)));
		assertTrue(withSet.hasMember(IntegerValue.make(98)));
		assertEquals(IntegerValue.one(), set.count());		
	}
	
	public void testWithout() {
		// Member element
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		ImmuSet withoutSet = set.without(IntegerValue.make(99));
		assertEquals(IntegerValue.zero(), withoutSet.count());
		assertEquals(IntegerValue.one(), set.count());
		
		// Non-member element
		withoutSet = set.without(IntegerValue.make(98));
		assertSame(set, withoutSet);
		assertEquals(IntegerValue.one(), withoutSet.count());
		assertEquals(IntegerValue.one(), set.count());
	}
	
	public void testHasMember() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertFalse(set.hasMember(IntegerValue.make(98)));
		assertFalse(set.hasMember(null));
	}
	
	public void testIsEmpty() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		assertFalse(set.isEmpty());		
	}
	
	public void testIsSubsetOf() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		assertTrue(set.isSubsetOf(set));
		assertFalse(set.isSubsetOf(ImmuSet.make()));		
		assertFalse(set.isSubsetOf(TinyImmuSet.make(IntegerValue.make(98))));
		
		MuSet muSet = TinyImmuSet.make(IntegerValue.make(99)).asMuSet();
		muSet.introduce(IntegerValue.make(98));
		muSet.introduce(IntegerValue.make(97));
		assertTrue(set.isSubsetOf(muSet));
		muSet.remove(IntegerValue.make(99));
		assertFalse(set.isSubsetOf(muSet));
	}
	
	public void testIntersect() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		ImmuSet intersectSet = set.intersect(set);
		assertSame(set, intersectSet);
		intersectSet = set.intersect(ImmuSet.make());
		assertTrue(intersectSet.isEmpty());		
		intersectSet = set.intersect(TinyImmuSet.make(IntegerValue.make(98)));
		assertTrue(intersectSet.isEmpty());		

		MuSet muSet = TinyImmuSet.make(IntegerValue.make(99)).asMuSet();
		muSet.introduce(IntegerValue.make(98));
		muSet.introduce(IntegerValue.make(97));
		intersectSet = set.intersect(muSet);
		assertSame(set, intersectSet);
		muSet.remove(IntegerValue.make(99));
		intersectSet = set.intersect(muSet);
		assertTrue(intersectSet.isEmpty());
	}

	public void testMinus() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		ImmuSet minusSet = set.minus(set);
		assertTrue(minusSet.isEmpty());		
		minusSet = set.minus(ImmuSet.make());
		assertSame(set, minusSet);
		minusSet = set.minus(TinyImmuSet.make(IntegerValue.make(98)));
		assertSame(set, minusSet);

		MuSet muSet = TinyImmuSet.make(IntegerValue.make(99)).asMuSet();
		muSet.introduce(IntegerValue.make(98));
		muSet.introduce(IntegerValue.make(97));
		minusSet = set.minus(muSet);
		assertTrue(minusSet.isEmpty());
		muSet.remove(IntegerValue.make(99));
		minusSet = set.minus(muSet);
		assertSame(set, minusSet);
	}

	public void testUnionWith() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		ImmuSet unionSet = set.unionWith(set);
		assertEquals(set, unionSet);
		unionSet = set.unionWith(ImmuSet.make());
		assertSame(set, unionSet);
		unionSet = set.unionWith(TinyImmuSet.make(IntegerValue.make(98)));
		assertEquals(IntegerValue.make(2), unionSet.count());
		assertTrue(unionSet.hasMember(IntegerValue.make(99)));
		assertTrue(unionSet.hasMember(IntegerValue.make(98)));

		MuSet muSet = TinyImmuSet.make(IntegerValue.make(99)).asMuSet();
		muSet.introduce(IntegerValue.make(98));
		muSet.introduce(IntegerValue.make(97));
		unionSet = set.unionWith(muSet);
		assertEquals(muSet.asImmuSet(), unionSet);
		MuSet muSet2 = (MuSet)muSet.copy();
		muSet2.remove(IntegerValue.make(99));
		unionSet = set.unionWith(muSet2);
		assertEquals(muSet.asImmuSet(), unionSet);
	}
	
	public void testAsMuSet() {
		ImmuSet set = TinyImmuSet.make(IntegerValue.make(99));
		MuSet muSet = set.asMuSet();
		assertEquals(IntegerValue.one(), muSet.count());
		assertTrue(muSet.hasMember(IntegerValue.make(99)));
	}
}
