/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.sets.tests;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class AllTests extends TestCase {

	public static void main(String[] args) {
		junit.textui.TestRunner.run(AllTests.class);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.collection.sets.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(ActualHashSetTest.class));
		suite.addTest(new TestSuite(EmptyImmuSetTest.class));
		suite.addTest(new TestSuite(HashSetTest.class));
		suite.addTest(new TestSuite(ImmuSetOnMuTest.class));
		suite.addTest(new TestSuite(ImmuSetTest.class));
		suite.addTest(new TestSuite(MuSetTest.class));
		suite.addTest(new TestSuite(ScruSetTest.class));
		suite.addTest(new TestSuite(SetAccumulatorTest.class));
		suite.addTest(new TestSuite(TinyImmuSetTest.class));
		suite.addTest(new TestSuite(UnionRecruiterTest.class));
		//$JUnit-END$
		return suite;
	}
}
