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
package info.dgjones.abora.white.xpp.basic;

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;

public abstract class Heaper {

	//////////////////////////////////////////////
	// Constructors

	public Heaper() {
		super();
	}

	//////////////////////////////////////////////
	// Comparison and Hashing

	//TODO review whether we should be  connecting into java
	public boolean equals(Object other) {
		if (other instanceof Heaper) {
			Heaper o = (Heaper) other;
			return isEqual(o);
		} else {
			return super.equals(other);
		}
	}

	/**
	 * Return true if the two objects are equal.
	 */
	public abstract boolean isEqual(Heaper other);
	//TODO use equals?
	/*
	Xanadu-Xpp-Basic.st:285:Heaper methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		"Return true if the two objects are equal."
		self subclassResponsibility!
	*/

	/**
	 * The value returned does not change during the life of the object.
	 */
	public int hashForEqual() {
		//TODO use hashCode?
		return actualHashForEqual();
	}

	/**
	 * Defined by subclasses to produce the value returned by hashForEqual.
	 */
	protected int actualHashForEqual() {
		return System.identityHashCode(this);
	}

	//////////////////////////////////////////////
	// Printing

	/**
	 * This should rarely be overridden.  In Tofu, it prints ClassName(...),
	 * where ... is either produced by printInsideOn or is ??? if printInsideOn
	 * it not overridden.
	 */
	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print('(');
		try {
			printContentsOn(oo);
		} catch (RuntimeException e) {
			oo.print("***PRINT BLASTED***");
		}
		oo.print(')');
	}

	/**
	 * Subclasses override this method to customize their printing.
	 */
	public void printContentsOn(PrintWriter oo) {
		oo.print("????");
	}

	public String toString() {
		//TODO performance concerns over the choice of PrintWriter
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream();
		PrintWriter printWriter = new PrintWriter(outputStream);
		printOn(printWriter);
		printWriter.flush();
		return outputStream.toString();
	}

	public void destroy() {
		//TODO probably dont need - just here to satisfy senders
		//TODO should this be stopped from being called more than once
		destruct();
	}

	public void destruct() {
		//TODO is this significant? - just here to satisfy senders
	}

	public void sendSelfTo(Xmtr xmtr) {
		throw new UnsupportedOperationException();
	}

	public Heaper(Rcvr receiver) {
		super();
		throw new UnsupportedOperationException();
	}
}
