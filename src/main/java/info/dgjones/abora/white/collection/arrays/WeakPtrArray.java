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
package info.dgjones.abora.white.collection.arrays;

import info.dgjones.abora.white.xpp.basic.Heaper;

//TODO need to know more about the implementation
//TODO should this extend SharedPtrArray
public class WeakPtrArray extends PtrArray {
	//TODO use Java weak ptr to implement

	//////////////////////////////////////////////
	// Constructors
	
	public WeakPtrArray(int count) {
		super(count);
		throw new UnsupportedOperationException();
	}

	public WeakPtrArray(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		super(size, from, sourceOffset, count, destOffset);
		throw new UnsupportedOperationException();
	}

	protected WeakPtrArray(Heaper[] buffer) {
		super(buffer);
		throw new UnsupportedOperationException();
	}

}
