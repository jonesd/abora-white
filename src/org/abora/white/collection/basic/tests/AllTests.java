package org.abora.white.collection.basic.tests;

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
		TestSuite suite = new TestSuite("Test for org.abora.white.collection.basic.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(IEEE32ArrayTest.class));
		//$JUnit-END$
		return suite;
	}
}
