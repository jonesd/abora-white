
/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package org.abora.white.collection.tables.tests;

import junit.framework.TestCase;

import org.abora.white.collection.steppers.TableAccumulator;
import org.abora.white.collection.tables.MuArray;
import org.abora.white.collection.tables.ScruTable;
import org.abora.white.spaces.integers.IntegerMapping;
import org.abora.white.value.IntegerValue;

public class MuArrayTest extends TestCase {

	public MuArrayTest(String arg0) {
		super(arg0);
	}

	public void testArray() {
		MuArray array = MuArray.array();
		assertTrue(array.isEmpty());
	}

	public void testArrayWith() {
		MuArray array = MuArray.array(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
	}

	public void testArrayWithWith() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
	}

	public void testArrayWithWithWith() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97));
		assertEquals(IntegerValue.make(3), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
		assertEquals(IntegerValue.make(97), array.intGet(IntegerValue.make(2)));
	}

	public void testArrayWithWithWithWith() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
		assertEquals(IntegerValue.make(4), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
		assertEquals(IntegerValue.make(97), array.intGet(IntegerValue.make(2)));
		assertEquals(IntegerValue.make(96), array.intGet(IntegerValue.make(3)));
	}
	
	public void testArrayAccumulator() {
		// Empty
		TableAccumulator accumulator = MuArray.arrayAccumulator();
		assertTrue(((MuArray)accumulator.value()).isEmpty());
		
		// After stepping
		accumulator.step(IntegerValue.make(99));
		accumulator.step(IntegerValue.make(98));
		MuArray array = (MuArray)accumulator.value();
		assertEquals(IntegerValue.make(2), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));
	}
	
	public void testArrayAccumulatorWith() {
		MuArray array = MuArray.array(IntegerValue.make(99));
		
		TableAccumulator accumulator = MuArray.arrayAccumulator(array);
		accumulator.step(IntegerValue.make(98));

		assertEquals(IntegerValue.make(2), array.count());
		assertEquals(IntegerValue.make(99), array.intGet(IntegerValue.zero()));
		assertEquals(IntegerValue.make(98), array.intGet(IntegerValue.one()));		
	}
	
	public void testMake() {
		MuArray array = (MuArray)MuArray.make(IntegerValue.make(10));
		assertTrue(array.isEmpty());
		//TODO do better in finding actual capacity
	}
	
	public void testOffsetScruArray() {
		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
		ScruTable offsetTable = MuArray.offsetScruArray(array, IntegerMapping.make(IntegerValue.make(-2)));
		assertEquals(IntegerValue.make(97), offsetTable.intGet(IntegerValue.zero()));
	}
	
//TODO
//	public void testOffsetSubTableBetween() {
//		MuArray array = MuArray.array(IntegerValue.make(99), IntegerValue.make(98), IntegerValue.make(97), IntegerValue.make(96));
//		ScruTable subtable = array.offsetSubTableBetween(IntegerValue.one(), IntegerValue.make(3), IntegerValue.make(0));
//		assertEquals(IntegerValue.make(2), subtable.count());
//		assertEquals(IntegerValue.make(98), subtable.intGet(IntegerValue.zero()));
//		assertEquals(IntegerValue.make(97), subtable.intGet(IntegerValue.one()));		
//	}
}
