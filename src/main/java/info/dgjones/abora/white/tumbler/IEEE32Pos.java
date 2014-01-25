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
import info.dgjones.abora.white.value.IEEE32Value;
import info.dgjones.abora.white.value.PrimFloatValue;

/**
 * For representing exactly those real numbers that can be represented in IEEE single
 * precision
 */
public class IEEE32Pos extends RealPos {
	protected float myValue;
	/*
	udanax-top.st:32160:
	RealPos subclass: #IEEE32Pos
		instanceVariableNames: 'myValue {IEEE32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:32164:
	IEEE32Pos comment:
	'For representing exactly those real numbers that can be represented in IEEE single precision'!
	*/
	/*
	udanax-top.st:32166:
	(IEEE32Pos getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public IEEE32Pos(float value) {
		super();
		myValue = value;
		/*
		udanax-top.st:32171:IEEE32Pos methodsFor: 'creation'!
		create: value {IEEE32}
			super create.
			myValue := value!
		*/
	}

	public double asIEEE() {
		return (double) myValue;
		//{
		//return myValue.basicCast(IEEE64);
		//}
		//translateOnly;
		///* >>> smalltalkOnly */
		//return myValue.asDouble();
		///* <<< smalltalkOnly */
		/*
		udanax-top.st:32178:IEEE32Pos methodsFor: 'obsolete:'!
		{IEEE64} asIEEE
			
			[^myValue basicCast: IEEE64] translateOnly.
			
			[^myValue asDouble] smalltalkOnly!
		*/
	}

	public double asIEEE64() {
		return (double) myValue;
		//		{
		//			return myValue.basicCast(IEEE64);
		//		}
		//		translateOnly;
		//		/* >>> smalltalkOnly */
		//		return myValue.asDouble();
		/* <<< smalltalkOnly */
		/*
		udanax-top.st:32184:IEEE32Pos methodsFor: 'obsolete:'!
		{IEEE64} asIEEE64
			
			[^myValue basicCast: IEEE64] translateOnly.
			
			[^myValue asDouble] smalltalkOnly!
		*/
	}

	public int precision() {
		return 32;
		/*
		udanax-top.st:32190:IEEE32Pos methodsFor: 'obsolete:'!
		{Int32} precision
			^32!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print("<");
		oo.print(myValue);
		oo.print(">");
		/*
		udanax-top.st:32196:IEEE32Pos methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << '<' << myValue << '>'!
		*/
	}

	public PrimFloatValue value() {
		return IEEE32Value.make(myValue);
		/*
		udanax-top.st:32202:IEEE32Pos methodsFor: 'accessing'!
		{PrimFloatValue} value
			^ PrimIEEE32 make: myValue!
		*/
	}

	public IEEE32Pos(Rcvr receiver) {
		super(receiver);
		myValue = (float) receiver.receiveIEEEDoubleVar();
		/*
		udanax-top.st:32207:IEEE32Pos methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myValue _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendIEEEDoubleVar(myValue);
		/*
		udanax-top.st:32211:IEEE32Pos methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myValue.!
		*/
	}
}
