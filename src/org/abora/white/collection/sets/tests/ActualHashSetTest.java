/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.sets.tests;

import junit.framework.TestCase;

import org.abora.white.collection.sets.ActualHashSet;
import org.abora.white.collection.sets.MuSet;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.value.IntegerValue;

public class ActualHashSetTest extends TestCase {

	public ActualHashSetTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		MuSet set = ActualHashSet.make();
		assertNotNull(set);
		assertTrue(set.isEmpty());
	}
	
	public void testMakeSize() {
		// Empty
		MuSet set = ActualHashSet.make(IntegerValue.zero());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// One
		set = ActualHashSet.make(IntegerValue.one());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// Many
		// One
		set = ActualHashSet.make(IntegerValue.make(23));
		assertNotNull(set);
		assertTrue(set.isEmpty());		
	}

	public void testMakeWith() {
		MuSet set = ActualHashSet.make(IntegerPos.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerPos.make(99)));
	}
	
	public void testContentsHash() {
		// Empty
		assertEquals(0, ActualHashSet.make().contentsHash());
		assertEquals(0, ActualHashSet.make(IntegerValue.make(20)).contentsHash());
		
		// More
		MuSet set = ActualHashSet.make(IntegerPos.make(1));
		int hash1 = set.contentsHash();
		set.introduce(IntegerPos.make(2));
		int hash2 = set.contentsHash();
		assertFalse(hash1 == 0);
		assertFalse(hash2 == 0);
		assertFalse(hash1 == hash2); 
	}
}
