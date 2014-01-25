/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package info.dgjones.abora.white.hash;

import junit.framework.TestCase;

import info.dgjones.abora.white.hash.FHash;

/**
 * @author jonesd
 */
public class FHashTest extends TestCase {

	public FHashTest(String arg0) {
		super(arg0);
	}

	public void testHashInt() {
		// 0
		assertTrue(FHash.hashInt(0) == 0);
		
		// strictly positive numbers
		assertFalse(FHash.hashInt(1000) == 1000);
		assertTrue(FHash.hashInt(1000) == FHash.hashInt(1000));
		assertFalse(FHash.hashInt(1000) == FHash.hashInt(1001));

		// negative numbers
		assertFalse(FHash.hashInt(-1000) == -1000);
		assertTrue(FHash.hashInt(-1000) == FHash.hashInt(-1000));
		assertFalse(FHash.hashInt(-1000) == FHash.hashInt(1000));
	}

	public void testHashDouble() {
		// 0
		assertTrue(FHash.hashDouble(0.0) == 0);
		
		// strictly positive numbers
		assertFalse(FHash.hashDouble(1000.1234) == 1000.1234);
		assertTrue(FHash.hashDouble(1000.1234) == FHash.hashDouble(1000.1234));
		assertFalse(FHash.hashDouble(1000.1234) == FHash.hashDouble(1000.1235));
		assertFalse(FHash.hashDouble(1000) == FHash.hashInt(1000));

		// negative numbers
		assertFalse(FHash.hashDouble(-1000.1234) == -1000.1234);
		assertTrue(FHash.hashDouble(-1000.1234) == FHash.hashDouble(-1000.1234));
		assertFalse(FHash.hashDouble(-1000.1234) == FHash.hashDouble(1000.1235));
		assertFalse(FHash.hashDouble(-1000) == FHash.hashInt(1000));
	}
}
