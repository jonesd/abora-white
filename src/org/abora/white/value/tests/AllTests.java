/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.white.value.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(AllTests.class);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.value.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(PrimFloatSpecTest.class));
		suite.addTest(new TestSuite(PrimIntegerSpecTest.class));
		suite.addTest(new TestSuite(PrimSpecTest.class));
		suite.addTest(new TestSuite(IntegerValueTest.class));
		//$JUnit-END$
		return suite;
	}
}
