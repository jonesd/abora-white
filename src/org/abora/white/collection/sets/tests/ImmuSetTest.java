/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.sets.tests;

import junit.framework.TestCase;

import org.abora.white.collection.sets.EmptyImmuSet;
import org.abora.white.collection.sets.ImmuSet;
import org.abora.white.collection.sets.MuSet;
import org.abora.white.collection.sets.TinyImmuSet;
import org.abora.white.value.IntegerValue;

public class ImmuSetTest extends TestCase {

	public ImmuSetTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(ImmuSetTest.class);
	}

	public void testMake() {
		ImmuSet set = ImmuSet.make();
		assertTrue(set.isEmpty());
	}

	public void testNewWith() {
		ImmuSet set = ImmuSet.newWith(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
	}
	
	public void testMakeWith() {
		EmptyImmuSet emptySet = (EmptyImmuSet)ImmuSet.make(MuSet.make());
		assertTrue(emptySet.isEmpty());
		
		MuSet startSet = MuSet.make();
		startSet.introduce(IntegerValue.make(99));
		TinyImmuSet tinySet = (TinyImmuSet)ImmuSet.make(startSet);
		assertEquals(IntegerValue.one(), tinySet.count());
		assertTrue(tinySet.hasMember(IntegerValue.make(99)));
		
		startSet.introduce(IntegerValue.make(98));
		ImmuSet set = ImmuSet.make(startSet);
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
	}

	public void testCopy() {
		ImmuSet set = ImmuSet.make();
		ImmuSet setCopy = (ImmuSet)set.copy();
		assertSame(set, setCopy);
		
		set = set.with(IntegerValue.make(99));
		setCopy = (ImmuSet)set.copy();
		assertSame(set, setCopy);

		set = set.with(IntegerValue.make(98));
		setCopy = (ImmuSet)set.copy();
		assertSame(set, setCopy);
	}

	public void testAsImmuSet() {
		ImmuSet set = ImmuSet.make();
		ImmuSet asSet = set.asImmuSet();
		assertSame(set, asSet);
		
		set = set.with(IntegerValue.make(99));
		asSet = set.asImmuSet();
		assertSame(set, asSet);

		set = set.with(IntegerValue.make(98));
		asSet = set.asImmuSet();
		assertSame(set, asSet);
	}
	
	public void testIsEqual() {
		ImmuSet set1 = ImmuSet.make();
		ImmuSet set2 = ImmuSet.make();
		assertTrue(set1.isEqual(set2));
		
		set1 = set1.with(IntegerValue.make(99));
		assertFalse(set1.isEqual(set2));
		
		set2 = set2.with(IntegerValue.make(99));
		assertTrue(set1.isEqual(set2));		

		set2 = set2.with(IntegerValue.make(98));
		assertFalse(set1.isEqual(set2));

		set1 = set1.with(IntegerValue.make(98));
		assertTrue(set1.isEqual(set2));
		
		// Not for MuSets
		MuSet muSet = MuSet.make();
		muSet.introduce(IntegerValue.make(99));
		muSet.introduce(IntegerValue.make(98));
		assertFalse(set1.isEqual(muSet));
		assertFalse(muSet.isEqual(set1));
		
		// Not for other types
		assertFalse(set1.isEqual(IntegerValue.make(99)));		
	}
	
	public void testActualHashForEqual() {
		ImmuSet set = ImmuSet.make();
		assertEquals(0, set.actualHashForEqual());
		
		ImmuSet set1 = set.with(IntegerValue.make(99));
		ImmuSet set1a = set.with(IntegerValue.make(98));
		ImmuSet set2 = set1.with(IntegerValue.make(98));
		ImmuSet set2a = set1a.with(IntegerValue.make(99));
		
		assertTrue(set1.actualHashForEqual() != set.actualHashForEqual());
		assertTrue(set1a.actualHashForEqual() != set1.actualHashForEqual());
		assertTrue(set2.actualHashForEqual() != set1a.actualHashForEqual());
		assertTrue(set2a.actualHashForEqual() == set2.actualHashForEqual());
	}
}
