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
package info.dgjones.abora.white.tumbler;

import java.io.PrintWriter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.value.IEEE64Value;
import info.dgjones.abora.white.value.PrimFloatValue;

/**
 * For representing exactly those real numbers that can be represented in IEEE double
 * precision
 */
public class IEEE64Pos extends RealPos {
	protected double myValue;
	/*
	udanax-top.st:32215:
	RealPos subclass: #IEEE64Pos
		instanceVariableNames: 'myValue {IEEE64}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:32219:
	IEEE64Pos comment:
	'For representing exactly those real numbers that can be represented in IEEE double precision'!
	*/
	/*
	udanax-top.st:32221:
	(IEEE64Pos getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public IEEE64Pos(double value) {
		super();
		myValue = value;
		/*
		udanax-top.st:32226:IEEE64Pos methodsFor: 'creation'!
		create: value {IEEE64}
			super create.
			myValue := value!
		*/
	}

	public double asIEEE() {
		return myValue;
		/*
		udanax-top.st:32233:IEEE64Pos methodsFor: 'obsolete:'!
		{IEEE64} asIEEE
			^myValue!
		*/
	}

	public double asIEEE64() {
		return myValue;
		/*
		udanax-top.st:32237:IEEE64Pos methodsFor: 'obsolete:'!
		{IEEE64} asIEEE64
			^myValue!
		*/
	}

	public int precision() {
		return 64;
		/*
		udanax-top.st:32241:IEEE64Pos methodsFor: 'obsolete:'!
		{Int32} precision
			^64!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print("<");
		oo.print(myValue);
		oo.print(">");
		/*
		udanax-top.st:32247:IEEE64Pos methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << '<' << myValue << '>'!
		*/
	}

	public PrimFloatValue value() {
		return IEEE64Value.make(myValue);
		/*
		udanax-top.st:32253:IEEE64Pos methodsFor: 'accessing'!
		{PrimFloatValue} value
			^ PrimIEEE64 make: myValue!
		*/
	}

	public IEEE64Pos(Rcvr receiver) {
		super(receiver);
		myValue = receiver.receiveIEEEDoubleVar();
		/*
		udanax-top.st:32258:IEEE64Pos methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myValue _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendIEEEDoubleVar(myValue);
		/*
		udanax-top.st:32262:IEEE64Pos methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myValue.!
		*/
	}
}
