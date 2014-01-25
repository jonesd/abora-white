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
package info.dgjones.abora.white.rcvr;

import info.dgjones.abora.white.collection.arrays.UInt8Array;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public abstract class Rcvr extends Heaper {
	//TODO should this be an Interface?
	/*
	udanax-top.st:41017:
	Heaper subclass: #Rcvr
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Xcvr'!
	*/
	/*
	udanax-top.st:41021:
	(Rcvr getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
	*/

	public abstract boolean receiveBooleanVar();
		/*
		udanax-top.st:41026:Rcvr methodsFor: 'receiving'!
		{BooleanVar} receiveBooleanVar
			self subclassResponsibility!
		*/

	/**
	 * Fill the array with data from the stream.
	 */
	public abstract void receiveData(UInt8Array array);
		/*
		udanax-top.st:41029:Rcvr methodsFor: 'receiving'!
		{void} receiveData: array {UInt8Array}
			"Fill the array with data from the stream."
			
			self subclassResponsibility!
		*/

	public abstract Heaper receiveHeaper();
		/*
		udanax-top.st:41034:Rcvr methodsFor: 'receiving'!
		{Heaper} receiveHeaper
			self subclassResponsibility!
		*/

	public abstract double receiveIEEEDoubleVar();
		/*
		udanax-top.st:41037:Rcvr methodsFor: 'receiving'!
		{IEEEDoubleVar} receiveIEEEDoubleVar
			self subclassResponsibility!
		*/

	public abstract int receiveInt32();
		/*
		udanax-top.st:41040:Rcvr methodsFor: 'receiving'!
		{Int32} receiveInt32
			self subclassResponsibility!
		*/

	public abstract byte receiveInt8();
		/*
		udanax-top.st:41043:Rcvr methodsFor: 'receiving'!
		{Int8} receiveInt8
			self subclassResponsibility!
		*/

	public abstract IntegerValue receiveIntegerVar();
		/*
		udanax-top.st:41046:Rcvr methodsFor: 'receiving'!
		{IntegerVar} receiveIntegerVar
			self subclassResponsibility!
		*/

	/**
	 * Receive an object into another object.
	 */
	public abstract void receiveInto(Heaper memory);
		/*
		udanax-top.st:41049:Rcvr methodsFor: 'receiving'!
		{void} receiveInto: memory {Heaper}
			"Receive an object into another object."
			self subclassResponsibility!
		*/

	public abstract String receiveString();
		/*
		udanax-top.st:41054:Rcvr methodsFor: 'receiving'!
		{char star} receiveString
			self subclassResponsibility!
		*/

	public abstract int receiveUInt32();
		/*
		udanax-top.st:41058:Rcvr methodsFor: 'receiving'!
		{UInt32} receiveUInt32
			self subclassResponsibility!
		*/

	public abstract byte receiveUInt8();
		/*
		udanax-top.st:41061:Rcvr methodsFor: 'receiving'!
		{UInt8} receiveUInt8
			self subclassResponsibility!
		*/

	/////////////////////////////////////////////
	// Comparing and Hashing
	
	public int actualHashForEqual() {
		return System.identityHashCode(this);
//		return asOop();
		/*
		udanax-top.st:41066:Rcvr methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:41068:Rcvr methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}
}
