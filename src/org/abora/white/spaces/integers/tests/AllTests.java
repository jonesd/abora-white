/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.spaces.integers.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.spaces.integers.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(AscendingIntegerStepperTest.class));
		suite.addTest(new TestSuite(IntegerRegionTest.class));
		//$JUnit-END$
		return suite;
	}
}
