/*
 * Created on 06-Mar-2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package org.abora.white.collection.tables.tests;

import junit.framework.Test;
import junit.framework.TestSuite;

/**
 * @author jonesd
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
public class AllTests {

	public static Test suite() {
		TestSuite suite = new TestSuite("Test for org.abora.white.collection.tables.tests");
		//$JUnit-BEGIN$
		suite.addTest(new TestSuite(PairTest.class));
		//$JUnit-END$
		return suite;
	}
}
