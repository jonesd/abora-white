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
package info.dgjones.abora.white.value;

import junit.framework.TestCase;

import info.dgjones.abora.white.value.PrimFloatValue;
import info.dgjones.abora.white.value.PrimSpec;

public class PrimFloatSpecTest extends TestCase {
	public PrimFloatSpecTest(String arg0) {
		super(arg0);
	}

	public void testValue() {
		PrimFloatValue value = PrimSpec.iEEE32().value(123.456);
		assertEquals(123.456f, value.asIEEE32(), 0.00001f);

		value = PrimSpec.iEEE64().value(123.456);
		assertEquals(123.456f, value.asIEEE64(), 0.00001f);
	}

	public void testIsEqual() {
		assertTrue(PrimSpec.iEEE32().isEqual(PrimSpec.iEEE32()));
		assertFalse(PrimSpec.iEEE32().isEqual(PrimSpec.iEEE64()));
		assertFalse(PrimSpec.iEEE32().isEqual(PrimSpec.int32()));		
	}
	
	public void testActualHashForEqual() {
		assertTrue(PrimSpec.iEEE32().actualHashForEqual() == PrimSpec.iEEE32().actualHashForEqual());
		assertFalse(PrimSpec.iEEE32().actualHashForEqual() == PrimSpec.iEEE64().actualHashForEqual());
		assertFalse(PrimSpec.iEEE32().actualHashForEqual() == PrimSpec.int32().actualHashForEqual());
	}
}
