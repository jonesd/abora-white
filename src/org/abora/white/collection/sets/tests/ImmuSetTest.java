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

}
