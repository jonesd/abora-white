/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.tables.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.collection.tables.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(PairTest.class));
		//$JUnit-END$
		return suite;
	}
}
