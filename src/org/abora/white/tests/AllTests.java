package org.abora.white.tests;

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
		TestSuite suite = new TestSuite("Test for org.abora.white.tests");

		suite.addTest(org.abora.white.collection.basic.tests.AllTests.suite());

		//$JUnit-BEGIN$
		//$JUnit-END$
		return suite;
	}
}
