/*
 * Created on 06-Mar-2003
 *
 * To change this generated comment go to 
 * Window>Preferences>Java>Code Generation>Code and Comments
 */
package org.abora.white.collection.tables.tests;

import junit.framework.TestCase;

import org.abora.white.collection.tables.Pair;
import org.abora.white.value.IntegerValue;

public class PairTest extends TestCase {

	/**
	 * Constructor for PairTest.
	 * @param arg0
	 */
	public PairTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.textui.TestRunner.run(PairTest.class);
	}

	public void testMake() {
		Pair p = Pair.make(IntegerValue.zero(), IntegerValue.one());
		assertEquals(IntegerValue.zero(), p.left());
		assertEquals(IntegerValue.one(), p.right());
		
		// Null params
		try {
			Pair.make(null, IntegerValue.one());
			fail("null,1");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			Pair.make(IntegerValue.zero(), null);
			fail("0,null");
		} catch (IllegalArgumentException e) {
			// expected
		}
		try {
			Pair.make(null, null);
			fail("null,null");
		} catch (IllegalArgumentException e) {
			// expected
		}
	}
	
	public void testReversed() {
		Pair p = Pair.make(IntegerValue.zero(), IntegerValue.one());
		Pair reversed = p.reversed();
				
		assertEquals(IntegerValue.zero(), p.left());
		assertEquals(IntegerValue.one(), p.right());
		
		assertEquals(IntegerValue.one(), reversed.left());
		assertEquals(IntegerValue.zero(), reversed.right());
	}

	public void testPrintOn() {
		Pair p = Pair.make(IntegerValue.zero(), IntegerValue.one());
		assertEquals("<0 , 1>", p.toString());		
	}
}
