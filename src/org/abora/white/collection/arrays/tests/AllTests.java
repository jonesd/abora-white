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
		suite.addTest(new TestSuite(Int16ArrayTest.class));
		suite.addTest(new TestSuite(Int32ArrayTest.class));
		suite.addTest(new TestSuite(Int64ArrayTest.class));
		suite.addTest(new TestSuite(UInt8ArrayTest.class));
		suite.addTest(new TestSuite(UInt16ArrayTest.class));
		suite.addTest(new TestSuite(UInt32ArrayTest.class));
		suite.addTest(new TestSuite(IntegerVarArrayTest.class));
		suite.addTest(new TestSuite(PtrArrayTest.class));		
		suite.addTest(new TestSuite(SharedPtrArrayTest.class));		
		//$JUnit-END$
		return suite;
	}
}
