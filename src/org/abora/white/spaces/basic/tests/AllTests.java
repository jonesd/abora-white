/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.spaces.basic.tests;

import junit.framework.Test;
import junit.framework.TestCase;
import junit.framework.TestSuite;

public class AllTests extends TestCase {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.spaces.basic.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(IntegerUpOrderTest.class));
		suite.addTest(new TestSuite(OrderSpecTest.class));
		suite.addTest(new TestSuite(ReverseOrderTest.class));
		//$JUnit-END$
		return suite;
	}
}
