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

public abstract class Xmtr extends Heaper {
	/*
	udanax-top.st:64227:
	Heaper subclass: #Xmtr
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Xcvr'!
	*/
	/*
	udanax-top.st:64231:
	(Xmtr getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
	*/

	/////////////////////////////////////////////
	// Sending
	
	public abstract void sendBooleanVar(boolean b);
	/*
	udanax-top.st:64236:Xmtr methodsFor: 'sending'!
	{void} sendBooleanVar: b {BooleanVar}
		self subclassResponsibility!
	*/

	public abstract void sendHeaper(Heaper object);
	/*
	udanax-top.st:64239:Xmtr methodsFor: 'sending'!
	{void} sendHeaper: object {Heaper}
		
		self subclassResponsibility!
	*/

	public abstract void sendIEEEDoubleVar(double x);
	/*
	udanax-top.st:64243:Xmtr methodsFor: 'sending'!
	{void} sendIEEEDoubleVar: x {IEEEDoubleVar}
		
		self subclassResponsibility!
	*/

	public abstract void sendInt32(int n);
	/*
	udanax-top.st:64247:Xmtr methodsFor: 'sending'!
	{void} sendInt32: n {Int32}
		
		self subclassResponsibility!
	*/

	public abstract void sendInt8(byte bytex);
	/*
	udanax-top.st:64251:Xmtr methodsFor: 'sending'!
	{void} sendInt8: byte {Int8}
		
		self subclassResponsibility!
	*/

	public abstract void sendIntegerVar(IntegerValue n);
	/*
	udanax-top.st:64255:Xmtr methodsFor: 'sending'!
	{void} sendIntegerVar: n {IntegerVar}
		
		self subclassResponsibility!
	*/

	public abstract void sendString(String s);
	/*
	udanax-top.st:64259:Xmtr methodsFor: 'sending'!
	{void} sendString: s {char star}
		self subclassResponsibility!
	*/

	public abstract void sendUInt32(int n);
	/*
	udanax-top.st:64263:Xmtr methodsFor: 'sending'!
	{void} sendUInt32: n {UInt32}
		
		self subclassResponsibility!
	*/

	public abstract void sendUInt8(byte bytex);
	/*
	udanax-top.st:64267:Xmtr methodsFor: 'sending'!
	{void} sendUInt8: byte {UInt8}
		
		self subclassResponsibility!
	*/

	public abstract void sendUInt8Data(UInt8Array array);
	/*
	udanax-top.st:64271:Xmtr methodsFor: 'sending'!
	{void} sendUInt8Data: array {UInt8Array}
		self subclassResponsibility!
	*/

	/**
	 * Dispatch to the send routines.
	 */
	public void send(Object object) {
		throw new UnsupportedOperationException();
//		if (object.isInteger()) {
//			sendIntegerVar(object);
//		} else {
//			if (object == true) {
//				sendUInt32(1);
//			} else {
//				if (object == false) {
//					sendUInt32(0);
//				} else {
//					sendHeaper(object);
//				}
//			}
//		}
		/*
		udanax-top.st:64277:Xmtr methodsFor: 'smalltalk: sending'!
		{void} send: object {Object}
			"Dispatch to the send routines."
			(object isInteger) ifTrue: [self sendIntegerVar: object]
			ifFalse: [object == true ifTrue: [self sendUInt32: 1]
			ifFalse: [object == false ifTrue: [self sendUInt32: UInt32Zero]
			ifFalse: [self sendHeaper: object]]]!
		*/
	}

	public abstract void sendData(UInt8Array array);
	/*
	udanax-top.st:64287:Xmtr methodsFor: 'smalltalk: deja vu'!
	{void} sendData: array {UInt8Array}
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Comparing and Hashing
	
	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//return asOop();
		/*
		udanax-top.st:64293:Xmtr methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Object other) {
		return this == other;
		/*
		udanax-top.st:64295:Xmtr methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}
}
