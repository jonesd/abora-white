/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.hash.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author jonesd
 */
public class AllTests {

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(AllTests.class);
	}

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.hash.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(FHashTest.class));
		//$JUnit-END$
		return suite;
	}
}
