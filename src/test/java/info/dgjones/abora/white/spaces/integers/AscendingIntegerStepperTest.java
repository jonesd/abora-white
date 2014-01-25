/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package info.dgjones.abora.white.spaces.integers;

import junit.framework.TestCase;

import info.dgjones.abora.white.collection.arrays.IntegerVarArray;
import info.dgjones.abora.white.spaces.integers.AscendingIntegerStepper;

public class AscendingIntegerStepperTest extends TestCase {

	public AscendingIntegerStepperTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		// Empty
		AscendingIntegerStepper stepper = AscendingIntegerStepper.make(IntegerVarArray.make(0), 0);
		assertTrue(stepper.atEnd());
	}
}
