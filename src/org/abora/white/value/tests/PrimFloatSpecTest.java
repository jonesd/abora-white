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

import junit.framework.TestCase;

import org.abora.white.value.PrimFloatSpec;
import org.abora.white.value.PrimFloatValue;
import org.abora.white.value.PrimSpec;

public class PrimFloatSpecTest extends TestCase {
	protected PrimFloatSpec spec;

	public PrimFloatSpecTest(String arg0) {
		super(arg0);
	}

	public static void main(String[] args) {
		junit.swingui.TestRunner.run(PrimFloatSpecTest.class);
	}
	
	public void setUp() {
		spec = PrimSpec.iEEE32();
	}
	
	public void testValue() {
		PrimFloatValue value = spec.value(0.0);
		
		
	}

}
