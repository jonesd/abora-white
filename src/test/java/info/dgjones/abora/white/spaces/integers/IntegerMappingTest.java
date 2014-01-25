/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * $Id$
 */
package info.dgjones.abora.white.spaces.integers;

import junit.framework.TestCase;

import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.integers.IntegerMapping;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.tumbler.IEEE32Pos;
import info.dgjones.abora.white.value.IntegerValue;

public class IntegerMappingTest extends TestCase {

	public IntegerMappingTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		IntegerMapping mapping = IntegerMapping.make();
		assertTrue(mapping.isIdentity());
	}
	
	public void testMakeWith() {
		// Identity
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.zero());
		assertTrue(mapping.isIdentity());

		// +1		
		mapping = IntegerMapping.make(IntegerValue.one());
		IntegerPos pos = (IntegerPos)mapping.of(IntegerPos.make(10));
		assertEquals(11, pos.asInt32());
		
		// -1
		mapping = IntegerMapping.make(IntegerValue.make(-1));
		pos = (IntegerPos)mapping.of(IntegerPos.make(10));
		assertEquals(9, pos.asInt32());		
	}

	public void testPrintOn() {
		IntegerMapping mapping = IntegerMapping.make();
		assertEquals("info.dgjones.abora.white.spaces.integers.IntegerMapping(0)", mapping.toString());		

		mapping = IntegerMapping.make(IntegerValue.make(-1));
		assertEquals("info.dgjones.abora.white.spaces.integers.IntegerMapping(-1)", mapping.toString());		

		mapping = IntegerMapping.make(IntegerValue.make(1));
		assertEquals("info.dgjones.abora.white.spaces.integers.IntegerMapping(1)", mapping.toString());		
	}
	
	public void testInverseOf() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.one());
		
		// Identity mapping
		IntegerPos pos = IntegerPos.make(10);
		IntegerPos inversePos = (IntegerPos)identity.inverseOf(pos);
		assertSame(pos, inversePos);
		
		// Mapping
		inversePos = (IntegerPos)mapping.inverseOf(pos);
		assertEquals(9, inversePos.asInt32());
		
		try {
			mapping.inverseOf(null);
			fail("null");
		} catch (IllegalArgumentException e) {
			// expected
		}
		
		try {
			mapping.inverseOf(IEEE32Pos.make(1.1));
			fail("ieee32");
		} catch (ClassCastException e) {
			// expected
		}
	}
	
	public void testInverseOfAll() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.one());

		// Identity
		XnRegion one = IntegerRegion.make(IntegerValue.one());
		XnRegion ofAll = identity.inverseOfAll(one);
		assertSame(one, ofAll);
			
		// Single
		ofAll = mapping.inverseOfAll(IntegerRegion.make(IntegerValue.make(10)));
		assertEquals(IntegerPos.make(9), ofAll.theOne());
			
		// Multiple
		ofAll = mapping.inverseOfAll(IntegerRegion.make(IntegerValue.make(-3), IntegerValue.make(5)));
		assertFalse(ofAll.hasMember(IntegerPos.make(-5)));
		assertTrue(ofAll.hasMember(IntegerPos.make(-4)));
		assertTrue(ofAll.hasMember(IntegerPos.make(3)));
		assertFalse(ofAll.hasMember(IntegerPos.make(4)));
	}

	public void testInverseOfInt() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.one());
		
		// Identity mapping
		IntegerValue value = IntegerValue.make(10);
		IntegerValue inverseValue = identity.inverseOfInt(value);
		assertSame(value, inverseValue);
		
		// Mapping
		inverseValue = mapping.inverseOfInt(value);
		assertEquals(9, inverseValue.asInt32());
		
		try {
			mapping.inverseOf(null);
			fail("null");
		} catch (IllegalArgumentException e) {
			// expected
		}
	}


	public void testOf() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.one());
		
		// Identity mapping
		IntegerPos pos = IntegerPos.make(10);
		IntegerPos inversePos = (IntegerPos)identity.of(pos);
		assertSame(pos, inversePos);
		
		// Mapping
		inversePos = (IntegerPos)mapping.of(pos);
		assertEquals(11, inversePos.asInt32());
		
		try {
			mapping.of(null);
			fail("null");
		} catch (IllegalArgumentException e) {
			// expected
		}
		
		try {
			mapping.of(IEEE32Pos.make(1.1));
			fail("ieee32");
		} catch (ClassCastException e) {
			// expected
		}
	}


	public void testOfAll() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.one());

		// Identity
		XnRegion one = IntegerRegion.make(IntegerValue.one());
		XnRegion ofAll = identity.ofAll(one);
		assertSame(one, ofAll);
			
		// Single
		ofAll = mapping.ofAll(IntegerRegion.make(IntegerValue.make(10)));
		assertEquals(IntegerPos.make(11), ofAll.theOne());
			
		// Multiple
		ofAll = mapping.ofAll(IntegerRegion.make(IntegerValue.make(-3), IntegerValue.make(5)));
		assertFalse(ofAll.hasMember(IntegerPos.make(-3)));
		assertTrue(ofAll.hasMember(IntegerPos.make(-2)));
		assertTrue(ofAll.hasMember(IntegerPos.make(5)));
		assertFalse(ofAll.hasMember(IntegerPos.make(6)));
	}


	public void testOfInt() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.one());
		
		// Identity mapping
		IntegerValue value = IntegerValue.make(10);
		IntegerValue inverseValue = identity.ofInt(value);
		assertSame(value, inverseValue);
		
		// Mapping
		inverseValue = mapping.ofInt(value);
		assertEquals(11, inverseValue.asInt32());
		
		try {
			mapping.ofInt(null);
			fail("null");
		} catch (NullPointerException e) {
			// expected
		}
	}
	
	public void testCoordinateSpace() {
		CoordinateSpace space = IntegerMapping.make().coordinateSpace();
		//TODO not quite sure what to test here
		assertTrue(space.verify(IntegerPos.make(10)));
	}
	
	public void testIsIdentity() {
		assertTrue(IntegerMapping.make().isIdentity());
		assertTrue(IntegerMapping.make(IntegerValue.zero()).isIdentity());

		assertFalse(IntegerMapping.make(IntegerValue.one()).isIdentity());
		assertFalse(IntegerMapping.make(IntegerValue.make(-1)).isIdentity());
	}
	
	public void testTranslation() {
		assertEquals(IntegerValue.zero(), IntegerMapping.make().translation());
		assertEquals(IntegerValue.one(), IntegerMapping.make(IntegerValue.one()).translation());
		assertEquals(IntegerValue.make(-1), IntegerMapping.make(IntegerValue.make(-1)).translation());
	}
	
	public void testActualHashForEqual() {
		int hash0 = IntegerMapping.make().actualHashForEqual();
		int hash1 = IntegerMapping.make(IntegerValue.one()).actualHashForEqual();
		int hash2 = IntegerMapping.make(IntegerValue.make(-1)).actualHashForEqual();
		
		assertTrue(hash0 != 0);
		assertTrue(hash0 != hash1);
		assertTrue(hash1 != hash2);
	}
	
	public void testIsEqual() {
		IntegerMapping identity = IntegerMapping.make();
		
		// Same
		assertTrue(identity.isEqual(identity));
		assertTrue(IntegerMapping.make(IntegerValue.make(99)).isEqual(IntegerMapping.make(IntegerValue.make(99))));
		
		// Different
		assertFalse(IntegerMapping.make(IntegerValue.make(99)).isEqual(IntegerMapping.make(IntegerValue.make(98))));
		assertFalse(IntegerMapping.make(IntegerValue.make(99)).isEqual(IntegerValue.make(99)));
	}
	
	public void testCompose() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.make(99));
		
		// Identity
		IntegerMapping composed = (IntegerMapping)identity.compose(mapping);
		assertSame(mapping, composed);
		composed = (IntegerMapping)mapping.compose(identity);
		assertSame(mapping, composed);
		
		// Composing
		composed = (IntegerMapping)mapping.compose(IntegerMapping.make(IntegerValue.one()));
		assertEquals(IntegerValue.make(100), composed.translation());
	}
	
	public void testInverse() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.make(99));
		
		// Identity
		IntegerMapping inverse = (IntegerMapping)identity.inverse();
		assertSame(identity, inverse);

		// Inverse
		inverse = (IntegerMapping)mapping.inverse();
		assertEquals(IntegerValue.make(-99), inverse.translation());
	}

	public void testInverseCompose() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.make(99));
		
		// Identity
		IntegerMapping composed = (IntegerMapping)identity.inverseCompose(mapping);
		assertSame(mapping, composed);
		
		// Composing
		composed = (IntegerMapping)mapping.inverseCompose(identity);
		assertEquals(IntegerValue.make(-99), composed.translation());

		composed = (IntegerMapping)mapping.inverseCompose(IntegerMapping.make(IntegerValue.one()));
		assertEquals(IntegerValue.make(-98), composed.translation());
	}

	public void testMinus() {
		IntegerMapping identity = IntegerMapping.make();
		IntegerMapping mapping = IntegerMapping.make(IntegerValue.make(99));
		
		// Identity
		IntegerMapping composed = (IntegerMapping)mapping.minus(identity);
		assertSame(mapping, composed);
		
		// Composing
		composed = (IntegerMapping)identity.minus(mapping);
		assertEquals(IntegerValue.make(-99), composed.translation());

		composed = (IntegerMapping)mapping.minus(IntegerMapping.make(IntegerValue.one()));
		assertEquals(IntegerValue.make(98), composed.translation());
	}
}
