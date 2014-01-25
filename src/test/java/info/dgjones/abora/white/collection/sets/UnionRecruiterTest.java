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

import info.dgjones.abora.white.collection.sets.ImmuSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.collection.sets.UnionRecruiter;
import info.dgjones.abora.white.collection.steppers.Accumulator;
import info.dgjones.abora.white.value.IntegerValue;

public class UnionRecruiterTest extends TestCase {

	public UnionRecruiterTest(String arg0) {
		super(arg0);
	}
	
	public void testMake() {
		UnionRecruiter recruiter = UnionRecruiter.make();
		ImmuSet set = (ImmuSet)recruiter.value();
		assertTrue(set.isEmpty());
	}

	public void testStep() {
		UnionRecruiter recruiter = UnionRecruiter.make();
		
		// Step empty
		recruiter.step(ImmuSet.make());
		
		// Step one
		MuSet set = MuSet.make();
		set.introduce(IntegerValue.make(99));
		recruiter.step(set);
		
		// Ensure recruiter not connected to previous set
		set.introduce(IntegerValue.make(100));
		
		// Step multiple with duplicate
		MuSet set2 = MuSet.make();
		set2.introduce(IntegerValue.make(98));
		set2.introduce(IntegerValue.make(99));
		recruiter.step(set2);
		
		// Non-set step
		try {
			recruiter.step(IntegerValue.make(101));
			fail("101");
		} catch (ClassCastException e) {
			// expected
		}
		
		ImmuSet value = (ImmuSet)recruiter.value();
		assertEquals(IntegerValue.make(2), value.count());
		assertTrue(value.hasMember(IntegerValue.make(99)));
		assertTrue(value.hasMember(IntegerValue.make(98)));		
	}

	public void testValue() {
		// Initial copy
		UnionRecruiter recruiter = UnionRecruiter.make();
		ImmuSet value0 = (ImmuSet)recruiter.value();

		// Step		
		MuSet set1 = MuSet.make();
		set1.introduce(IntegerValue.make(99));
		recruiter.step(set1);
		ImmuSet value1 = (ImmuSet)recruiter.value();
		
		// Step both accumulators
		MuSet set2 = MuSet.make();
		set2.introduce(IntegerValue.make(98));
		recruiter.step(set2);
		ImmuSet value2 = (ImmuSet)recruiter.value();

		// Ensure values are correct and separate
		assertTrue(value0.isEmpty());
		
		assertEquals(IntegerValue.one(), value1.count());
		assertTrue(value1.hasMember(IntegerValue.make(99)));
		
		assertEquals(IntegerValue.make(2), value2.count());
		assertTrue(value2.hasMember(IntegerValue.make(99)));
		assertTrue(value2.hasMember(IntegerValue.make(98)));		
	}

	public void testCopy() {
		// Initial copy
		UnionRecruiter recruiter = UnionRecruiter.make();
		
		// Copy after first step
		MuSet set1 = MuSet.make();
		set1.introduce(IntegerValue.make(99));
		recruiter.step(set1);
		Accumulator recruiterCopy = recruiter.copy();
		
		// Step both accumulators
		MuSet set2 = MuSet.make();
		set2.introduce(IntegerValue.make(98));
		recruiter.step(set2);

		MuSet set3 = MuSet.make();
		set3.introduce(IntegerValue.make(100));
		recruiterCopy.step(set3);
				
		// Ensure accumulators have stepped separately
		ImmuSet set = (ImmuSet)recruiter.value();
		assertEquals(IntegerValue.make(2), set.count());
		assertTrue(set.hasMember(IntegerValue.make(99)));
		assertTrue(set.hasMember(IntegerValue.make(98)));		

		ImmuSet setCopy = (ImmuSet)recruiterCopy.value();
		assertEquals(IntegerValue.make(2), setCopy.count());
		assertTrue(setCopy.hasMember(IntegerValue.make(99)));
		assertTrue(setCopy.hasMember(IntegerValue.make(100)));		
	}
}
