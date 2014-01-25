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

import info.dgjones.abora.white.xpp.basic.Heaper;

public abstract class PrimFloatValue extends PrimValue {

	protected PrimFloatValue() {
		super();
	}

	/**
	 * The value as an IEEE 32-bit floating point number.
	 * May not be possible if conversion from subclass to IEEE type is not available.
	 */
	public abstract float asIEEE32()
	/*
	all.st:36754:PrimFloatValue methodsFor: 'accessing'!
	{IEEE32} asIEEE32
		"The value as an IEEE 32-bit floating point number.
		May not be possible if conversion from subclass to IEEE type is not available."
	
		self subclassResponsibility!
	*/
	;
	
	/**
	 * Return the value as an IEEE 64-bit floating point number.
	 * 
	 * @return double value as an IEEE 64-bit floating point number.
	 */
	public abstract double asIEEE64();

	//TODO what does ug do?
	public boolean isEqual(Heaper other) {
		if (other instanceof PrimFloatValue) {
			PrimFloatValue otherFloat = (PrimFloatValue) other;
			return this.asIEEE64() == otherFloat.asIEEE64();
		} else {
			return false;
		}
	}

}
