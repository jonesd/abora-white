/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.sets.tests;

import junit.framework.TestCase;

import org.abora.white.collection.sets.HashSet;
import org.abora.white.collection.sets.MuSet;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.value.IntegerValue;

public class HashSetTest extends TestCase {

	public HashSetTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		MuSet set = HashSet.make();
		assertTrue(set.isEmpty());
	}

	public void testMakeSize() {
		// Empty
		MuSet set = HashSet.make(IntegerValue.zero());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// One
		set = HashSet.make(IntegerValue.one());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// Many
		set = HashSet.make(IntegerValue.make(23));
		assertNotNull(set);
		assertTrue(set.isEmpty());		
	}

	public void testMakeWith() {
		MuSet set = HashSet.make(IntegerPos.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerPos.make(99)));
	}
}
