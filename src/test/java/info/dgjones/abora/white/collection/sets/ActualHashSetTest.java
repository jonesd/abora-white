/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.white.collection.sets;

import junit.framework.TestCase;

import info.dgjones.abora.white.collection.sets.ActualHashSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class ActualHashSetTest extends TestCase {

	public ActualHashSetTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		MuSet set = ActualHashSet.make();
		assertNotNull(set);
		assertTrue(set.isEmpty());
	}
	
	public void testMakeSize() {
		// Empty
		MuSet set = ActualHashSet.make(IntegerValue.zero());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// One
		set = ActualHashSet.make(IntegerValue.one());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// Many
		set = ActualHashSet.make(IntegerValue.make(23));
		assertNotNull(set);
		assertTrue(set.isEmpty());		
	}

	public void testMakeWith() {
		MuSet set = ActualHashSet.make(IntegerPos.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerPos.make(99)));
	}
	
	public void testContentsHash() {
		// Empty
		assertEquals(0, ActualHashSet.make().contentsHash());
		assertEquals(0, ActualHashSet.make(IntegerValue.make(20)).contentsHash());
		
		// More
		MuSet set = ActualHashSet.make(IntegerPos.make(1));
		int hash1 = set.contentsHash();
		set.introduce(IntegerPos.make(2));
		int hash2 = set.contentsHash();
		assertFalse(hash1 == 0);
		assertFalse(hash2 == 0);
		assertFalse(hash1 == hash2); 
	}
	
	public void testCount() {
		MuSet set = ActualHashSet.make();
		assertEquals(IntegerValue.zero(), set.count());
		set.introduce(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		set.introduce(IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), set.count());
	}
	
	public void testHasMember() {
		MuSet set = ActualHashSet.make();
		assertFalse(set.hasMember(IntegerValue.make(99)));
		set.introduce(IntegerValue.make(99));
		assertTrue(set.hasMember(IntegerValue.make(99)));
		set.introduce(IntegerValue.make(98));
		assertTrue(set.hasMember(IntegerValue.make(99)));
		set.remove(IntegerValue.make(99));
		assertFalse(set.hasMember(IntegerValue.make(99)));
	}
	
	public void testIsEmpty() {
		MuSet set = ActualHashSet.make();
		assertTrue(set.isEmpty());
		set.introduce(IntegerValue.make(99));
		assertFalse(set.isEmpty());
		set.introduce(IntegerValue.make(98));
		assertFalse(set.isEmpty());
		set.remove(IntegerValue.make(99));
		assertFalse(set.isEmpty());
		set.remove(IntegerValue.make(98));
		assertTrue(set.isEmpty());		
	}

	public void testCopy() {
		MuSet set = ActualHashSet.make();
		MuSet set1 = (MuSet)set.copy();
		assertNotSame(set, set1);
		assertTrue(set.isEmpty());
		assertTrue(set1.isEmpty());
		assertTrue(set.contentsEqual(set1));
		
		set.introduce(IntegerValue.make(99));
		MuSet set2 = (MuSet)set.copy();
		assertNotSame(set, set2);
		assertTrue(set.contentsEqual(set2));
		assertFalse(set.contentsEqual(set1));

		set.introduce(IntegerValue.make(98));
		MuSet set3 = (MuSet)set.copy();
		assertNotSame(set, set3);
		assertTrue(set.contentsEqual(set3));
		assertFalse(set.contentsEqual(set2));
	}
	
	public void testStepper() {
		// Empty
		MuSet set = ActualHashSet.make();
		Stepper stepper = set.stepper();
		assertTrue(stepper.atEnd());
		
		// First element
		set.introduce(IntegerValue.make(99));
		stepper = set.stepper();
		assertEquals(IntegerValue.make(99), stepper.get());
		stepper.step();
		assertTrue(stepper.atEnd());

		// Second element
		set.introduce(IntegerValue.make(98));
		stepper = set.stepper();
		Heaper found1 = stepper.get();
		stepper.step();
		Heaper found2 = stepper.get();
		stepper.step();
		assertTrue(stepper.atEnd());
		assertTrue(!found1.isEqual(found2));
		assertTrue(found1.isEqual(IntegerValue.make(99)) || found1.isEqual(IntegerValue.make(98)));
		assertTrue(found2.isEqual(IntegerValue.make(99)) || found2.isEqual(IntegerValue.make(98)));
	}
	
	public void testTheOne() {
		// Empty
		MuSet set = ActualHashSet.make();
		try {
			set.theOne();
			fail("empty");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_ONE_ELEMENT, e.getMessage());
		}
		
		// First element
		set.introduce(IntegerValue.make(99));
		assertEquals(IntegerValue.make(99), set.theOne());

		// Second element
		set.introduce(IntegerValue.make(98));
		try {
			set.theOne();
			fail("2");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_ONE_ELEMENT, e.getMessage());
		}
	}
	
	public void testStoreAll() {
		// Empty
		MuSet set = ActualHashSet.make();
		set.storeAll(ActualHashSet.make());
		assertTrue(set.isEmpty());
		
		// First element
		MuSet set1 = ActualHashSet.make();
		set1.introduce(IntegerValue.make(99));
		set.storeAll(set1);
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		
		// Duplicate first element and second
		set1.introduce(IntegerValue.make(98));
		set.storeAll(set1);
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		
		// Full duplication		
		set.storeAll(set1);
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		
		// Add nothing
		set.storeAll(ActualHashSet.make());
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		
		//TODO check case when source is full before storeAlling
	}
	
	public void testWipeAll() {
		// Empty
		MuSet set = ActualHashSet.make();
		set.wipeAll(ActualHashSet.make());
		assertTrue(set.isEmpty());
		
		set.introduce(IntegerValue.make(99));
		set.introduce(IntegerValue.make(98));
		
		// No Matches
		MuSet set1 = ActualHashSet.make();
		set1.introduce(IntegerValue.make(90));
		set1.introduce(IntegerValue.make(89));
		set.wipeAll(set1);
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		// One match
		set1.introduce(IntegerValue.make(98));	
		set.wipeAll(set1);
		assertEquals(IntegerValue.make(1), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));

		set.introduce(IntegerValue.make(98));
	

		// All match
		set1.introduce(IntegerValue.make(99));	
		set.wipeAll(set1);
		assertTrue(set.isEmpty());
		
		assertEquals(IntegerValue.make(4), set1.count());
	}
	
	public void testIntroduce() {
		// Empty
		MuSet set = ActualHashSet.make();		
		set.introduce(IntegerValue.make(99));
		
		// First
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		
		// Second
		set.introduce(IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		
		// Duplicate
		try {
			set.introduce(IntegerValue.make(98));
			fail("duplicate");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.ALREADY_IN_SET, e.getMessage());
		}
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		
		// Force capacity increase
		for (int i = 100; i < 200; i++) {
			set.introduce(IntegerValue.make(i));
		}
		assertEquals(IntegerValue.make(102), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		for (int i = 100; i < 200; i++) {
			assertTrue(set.hasMember(IntegerValue.make(i)));
		}				
	}
	
	public void testRemove() {
		// Empty
		MuSet set = ActualHashSet.make();
		try {		
			set.remove(IntegerValue.make(99));
			fail("empty");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_IN_SET, e.getMessage());
		}
		
		set.introduce(IntegerValue.make(99));
		set.introduce(IntegerValue.make(98));

		// Remove element
		set.remove(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertFalse(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		// Remove same element again
		try {		
			set.remove(IntegerValue.make(99));
			fail("99");
		} catch (AboraRuntimeException e) {
			assertEquals(AboraRuntimeException.NOT_IN_SET, e.getMessage());
		}
		assertEquals(IntegerValue.one(), set.count());
		assertFalse(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		// Last element
		set.remove(IntegerValue.make(98));
		assertTrue(set.isEmpty());

		// Stress Test
		// Force capacity increase
		for (int i = 100; i < 200; i++) {
			set.introduce(IntegerValue.make(i));
		}
		for (int i = 100; i < 200; i++) {
			set.remove(IntegerValue.make(i));
		}
		assertTrue(set.isEmpty());				
	}

	public void testStore() {
		MuSet set = ActualHashSet.make();		
		
		// First
		set.store(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		
		// Second
		set.store(IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		
		// Duplicate
		set.store(IntegerValue.make(98));
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		// Force capacity increase
		for (int i = 100; i < 200; i++) {
			set.store(IntegerValue.make(i));
		}
		assertEquals(IntegerValue.make(102), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));
		for (int i = 100; i < 200; i++) {
			assertTrue(set.hasMember(IntegerValue.make(i)));
		}				
	}

	public void testWipe() {
		// Empty
		MuSet set = ActualHashSet.make();
		set.wipe(IntegerValue.make(99));
		assertTrue(set.isEmpty());
		
		set.introduce(IntegerValue.make(99));
		set.introduce(IntegerValue.make(98));

		// Remove element
		set.wipe(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertFalse(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		// Remove same element again
		set.wipe(IntegerValue.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertFalse(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));

		// Last element
		set.wipe(IntegerValue.make(98));
		assertTrue(set.isEmpty());
	}
}
