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
package info.dgjones.abora.white.collection.cache;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class HashSetCache extends Heaper {
	protected int mySize;
	protected PtrArray myElements;
	/*
	udanax-top.st:26942:
	Heaper subclass: #HashSetCache
		instanceVariableNames: '
			mySize {UInt32}
			myElements {PtrArray}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Cache'!
	*/
	/*
	udanax-top.st:26948:
	(HashSetCache getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:27005:
	HashSetCache class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:27008:
	(HashSetCache getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #COPY; yourself)!
	*/

	public boolean hasMember(Heaper aHeaper) {
		int index = aHeaper.hashForEqual() % mySize;
		if (index < 0 || (index >= mySize)) {
			throw new AboraRuntimeException(AboraRuntimeException.MODULO_FAILED);
		}
		Heaper val = myElements.fetch(index);
		return val != null && (aHeaper.isEqual(val));
		/*
		udanax-top.st:26953:HashSetCache methodsFor: 'accessing'!
		{BooleanVar} hasMember: aHeaper {Heaper}
			| index {UInt32 register} val {Heaper | NULL} |
			index _ aHeaper hashForEqual \\ mySize.
			(index < UInt32Zero or: [index >= mySize]) ifTrue: [Heaper BLAST: #ModuloFailed].
			val _ myElements fetch: index.
			^val ~~ NULL and: [aHeaper isEqual: val]!
		*/
	}

	public void store(Heaper aHeaper) {
		int index = aHeaper.hashForEqual() % mySize;
		if (index < 0 || (index >= mySize)) {
			throw new AboraRuntimeException(AboraRuntimeException.MODULO_FAILED);
		}
		myElements.store(index, aHeaper);
		/*
		udanax-top.st:26960:HashSetCache methodsFor: 'accessing'!
		{void} store: aHeaper {Heaper} 
			| index {UInt32 register} |
			index _ aHeaper hashForEqual \\ mySize.
			(index < UInt32Zero or: [index >= mySize]) ifTrue: [Heaper BLAST: #ModuloFailed].
			myElements at: index store: aHeaper!
		*/
	}

	public void wipe(Heaper aHeaper) {
		int index = aHeaper.hashForEqual() % mySize;
		if (index < 0 || (index >= mySize)) {
			throw new AboraRuntimeException(AboraRuntimeException.MODULO_FAILED);
		}
		Heaper val = myElements.fetch(index);
		if (val != null && (aHeaper.isEqual(val))) {
			myElements.store(index, null);
		}
		/*
		udanax-top.st:26966:HashSetCache methodsFor: 'accessing'!
		{void} wipe: aHeaper {Heaper} 
			| index {UInt32 register} val {Heaper | NULL} |
			index _ aHeaper hashForEqual \\ mySize.
			(index < UInt32Zero or: [index >= mySize]) ifTrue: [Heaper BLAST: #ModuloFailed].
			val _ myElements fetch: index.
			(val ~~ NULL and: [aHeaper isEqual: val])
				ifTrue: [myElements at: index store: NULL]!
		*/
	}

	public HashSetCache(int size) {
		super();
		mySize = size;
		myElements = PtrArray.make(mySize);
		/*
		udanax-top.st:26976:HashSetCache methodsFor: 'create/delete'!
		create: size {UInt32}
			super create.
			mySize _ size.
			myElements _ PtrArray nulls: mySize!
		*/
	}

	public void destruct() {
		myElements = null;
		mySize = 0;
		super.destruct();
		/*
		udanax-top.st:26983:HashSetCache methodsFor: 'protected: creation'!
		{void} destruct
			myElements _ NULL.
			mySize _ UInt32Zero.
			super destruct!
		*/
	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
//		return asOop();
		/*
		udanax-top.st:26990:HashSetCache methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public HashSetCache(Rcvr receiver) {
		super(receiver);
		mySize = receiver.receiveUInt32();
		myElements = (PtrArray)receiver.receiveHeaper();
		/*
		udanax-top.st:26992:HashSetCache methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			mySize _ receiver receiveUInt32.
			myElements _ receiver receiveHeaper.!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:26997:HashSetCache methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendUInt32(mySize);
		xmtr.sendHeaper(myElements);
		/*
		udanax-top.st:26999:HashSetCache methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendUInt32: mySize.
			xmtr sendHeaper: myElements.!
		*/
	}

	public static Heaper make() {
		return new HashSetCache(10);
		/*
		udanax-top.st:27013:HashSetCache class methodsFor: 'pseudo-constructors'!
		make
			^self create: 10!
		*/
	}

	public static Heaper make(int size) {
		return new HashSetCache(size);
		/*
		udanax-top.st:27016:HashSetCache class methodsFor: 'pseudo-constructors'!
		make: size {UInt32}
			^self create: size!
		*/
	}
}
