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
import org.abora.white.value.IntegerValue;

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
}
