/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(AllTests.class);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.tests");

		suite.addTest(org.abora.white.collection.arrays.tests.AllTests.suite());
		suite.addTest(org.abora.white.collection.sets.tests.AllTests.suite());
		suite.addTest(org.abora.white.collection.tables.tests.AllTests.suite());
		suite.addTest(org.abora.white.hash.tests.AllTests.suite());
		suite.addTest(org.abora.white.value.tests.AllTests.suite());

		//$JUnit-BEGIN$
		//$JUnit-END$
		return suite;
	}
}
