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

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.sets.ImmuSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class ScruSetTest extends TestCase {

	public ScruSetTest(String arg0) {
		super(arg0);
	}

	public void testContentsEqual() {
		// Empty same type
		MuSet muSet1 = MuSet.make();
		MuSet muSet2 = MuSet.make();
		assertTrue(muSet1.contentsEqual(muSet2));

		ImmuSet immuSet1 = ImmuSet.make();
		ImmuSet immuSet2 = ImmuSet.make();
		assertTrue(immuSet1.contentsEqual(immuSet2));

		// Empty different type
		assertTrue(immuSet1.contentsEqual(muSet1));
		assertTrue(muSet1.contentsEqual(immuSet1));

		// Matching
		muSet1.introduce(IntegerValue.make(99));
		muSet1.introduce(IntegerValue.make(98));
		immuSet1 = immuSet1.with(IntegerValue.make(98));
		immuSet1 = immuSet1.with(IntegerValue.make(99));
		assertTrue(muSet1.contentsEqual(immuSet1));

		// Different - same size
		muSet1.introduce(IntegerValue.make(97));
		assertFalse(muSet1.contentsEqual(immuSet1));

		// Different - different size
		immuSet1 = immuSet1.with(IntegerValue.make(100));
		assertFalse(muSet1.contentsEqual(immuSet1));
	}

	public void testContentsHash() {
		// Empty same type
		MuSet muSet1 = MuSet.make();
		MuSet muSet2 = MuSet.make();
		assertEquals(0, muSet1.contentsHash());
		assertEquals(muSet1.contentsHash(), muSet2.contentsHash());

		ImmuSet immuSet1 = ImmuSet.make();
		ImmuSet immuSet2 = ImmuSet.make();
		assertEquals(0, immuSet1.contentsHash());
		assertEquals(immuSet1.contentsHash(), immuSet2.contentsHash());

		// Matching single (remember TinyImmuSet optimization)
		muSet1.introduce(IntegerValue.make(99));
		immuSet1 = immuSet1.with(IntegerValue.make(99));
		assertTrue(0 != immuSet1.contentsHash());
		assertEquals(muSet1.contentsHash(), immuSet1.contentsHash());

		// Matching mulitple elements
		muSet1.introduce(IntegerValue.make(98));
		muSet1.introduce(IntegerValue.make(97));
		immuSet1 = immuSet1.with(IntegerValue.make(97));
		immuSet1 = immuSet1.with(IntegerValue.make(98));
		assertTrue(0 != immuSet1.contentsHash());
		assertEquals(muSet1.contentsHash(), immuSet1.contentsHash());

		// Different - same size
		muSet1.introduce(IntegerValue.make(96));
		assertTrue(muSet1.contentsHash() != immuSet1.contentsHash());

		// Different - different size
		immuSet1 = immuSet1.with(IntegerValue.make(100));
		assertTrue(muSet1.contentsHash() != immuSet1.contentsHash());
	}

	public void testIntersects() {
		// Empty
		MuSet muSet1 = MuSet.make();
		ImmuSet immuSet1 = ImmuSet.make();
		assertFalse(muSet1.intersects(immuSet1));
		assertFalse(immuSet1.intersects(muSet1));

		// No-intersection
		muSet1.introduce(IntegerValue.make(99));
		immuSet1 = immuSet1.with(IntegerValue.make(98));
		assertFalse(muSet1.intersects(immuSet1));
		assertFalse(immuSet1.intersects(muSet1));

		// Single intersection
		muSet1.introduce(IntegerValue.make(98));
		assertTrue(muSet1.intersects(immuSet1));
		assertTrue(immuSet1.intersects(muSet1));
	}

	public void testIsSubsetOf() {
		// Empty
		MuSet muSet1 = MuSet.make();
		ImmuSet immuSet1 = ImmuSet.make();
		assertTrue(muSet1.isSubsetOf(immuSet1));
		assertTrue(immuSet1.isSubsetOf(muSet1));

		// Subset with single element
		muSet1.introduce(IntegerValue.make(99));
		immuSet1 = immuSet1.with(IntegerValue.make(99));
		assertTrue(muSet1.isSubsetOf(immuSet1));
		assertTrue(immuSet1.isSubsetOf(muSet1));

		muSet1.introduce(IntegerValue.make(98));
		assertFalse(muSet1.isSubsetOf(immuSet1));
		assertTrue(immuSet1.isSubsetOf(muSet1));
	}

	public void testAsArrayMu() {
		// Empty
		MuSet muSet = MuSet.make();
		PtrArray ptrArray = muSet.asArray();
		assertEquals(0, ptrArray.count());

		muSet.introduce(IntegerValue.make(99));
		ptrArray = muSet.asArray();
		assertEquals(1, ptrArray.count());
		assertEquals(IntegerValue.make(99), ptrArray.fetch(0));

		muSet.introduce(IntegerValue.make(98));
		ptrArray = muSet.asArray();
		assertEquals(2, ptrArray.count());
		Heaper heaper1 = ptrArray.fetch(0);
		Heaper heaper2 = ptrArray.fetch(1);
		assertTrue(heaper1.isEqual(IntegerValue.make(99)) || heaper1.isEqual(IntegerValue.make(98)));
		assertTrue(heaper2.isEqual(IntegerValue.make(99)) || heaper2.isEqual(IntegerValue.make(98)));
	}

	public void testAsArrayImmu() {
		// Empty
		ImmuSet immuSet = ImmuSet.make();
		PtrArray ptrArray = immuSet.asArray();
		assertEquals(0, ptrArray.count());

		immuSet = immuSet.with(IntegerValue.make(99));
		ptrArray = immuSet.asArray();
		assertEquals(1, ptrArray.count());
		assertEquals(IntegerValue.make(99), ptrArray.fetch(0));

		immuSet = immuSet.with(IntegerValue.make(98));
		ptrArray = immuSet.asArray();
		assertEquals(2, ptrArray.count());
		Heaper heaper1 = ptrArray.fetch(0);
		Heaper heaper2 = ptrArray.fetch(1);
		assertTrue(heaper1.isEqual(IntegerValue.make(99)) || heaper1.isEqual(IntegerValue.make(98)));
		assertTrue(heaper2.isEqual(IntegerValue.make(99)) || heaper2.isEqual(IntegerValue.make(98)));
	}

	public void testPrintString() {
		// Empty
		MuSet set = MuSet.make();
		assertEquals("info.dgjones.abora.white.collection.sets.ActualHashSet{nullSet}", set.toString());

		// One element
		set.introduce(IntegerValue.make(99));
		assertEquals("info.dgjones.abora.white.collection.sets.ActualHashSet{99}", set.toString());

		// Two elements
		set.introduce(IntegerValue.make(98));
		assertTrue(
			"info.dgjones.abora.white.collection.sets.ActualHashSet{99, 98}".equals(set.toString())
				|| "info.dgjones.abora.white.collection.sets.ActualHashSet{98, 99}".equals(set.toString()));

		// Too long string
		for (int i = 100; i < 299; i++) {
			set.introduce(IntegerValue.make(i));
		}
		assertTrue(set.toString().indexOf("etc...") == -1);
		set.introduce(IntegerValue.make(4));
		assertTrue(set.toString().indexOf("etc...") != -1);
		//TODO could improve this test to ensure no extra members are printed
	}
}