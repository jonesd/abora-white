/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.tables.tests;

import junit.framework.TestCase;

import org.abora.white.collection.tables.ActualArray;
import org.abora.white.collection.tables.MuArray;
import org.abora.white.value.IntegerValue;

public class ActualArrayTest extends TestCase {

	public ActualArrayTest(String arg0) {
		super(arg0);
	}

	public void testIsEmpty() {
		//Empty
		ActualArray array = (ActualArray)MuArray.array();
		assertTrue(array.isEmpty());
		
		array = (ActualArray) MuArray.array(IntegerValue.make(99));
		assertFalse(array.isEmpty());
	}

}
