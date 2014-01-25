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

import info.dgjones.abora.white.collection.sets.HashSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.value.IntegerValue;

public class HashSetTest extends TestCase {

	public HashSetTest(String arg0) {
		super(arg0);
	}

	public void testMake() {
		MuSet set = HashSet.make();
		assertTrue(set.isEmpty());
	}

	public void testMakeSize() {
		// Empty
		MuSet set = HashSet.make(IntegerValue.zero());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// One
		set = HashSet.make(IntegerValue.one());
		assertNotNull(set);
		assertTrue(set.isEmpty());
		
		// Many
		set = HashSet.make(IntegerValue.make(23));
		assertNotNull(set);
		assertTrue(set.isEmpty());		
	}

	public void testMakeWith() {
		MuSet set = HashSet.make(IntegerPos.make(99));
		assertEquals(IntegerValue.one(), set.count());
		assertTrue(set.hasMember(IntegerPos.make(99)));
	}
}
