/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.arrays.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(AllTests.class);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.collection.arrays.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(IEEE32ArrayTest.class));
		suite.addTest(new TestSuite(IEEE64ArrayTest.class));
		suite.addTest(new TestSuite(Int8ArrayTest.class));
		//$JUnit-END$
		return suite;
	}
}
