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
package info.dgjones.abora.white.collection.settable;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class HashIndexEntry extends TableEntry {
	/*
	udanax-top.st:56590:
	TableEntry subclass: #HashIndexEntry
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-SetTable'!
	*/
	/*
	udanax-top.st:56594:
	(HashIndexEntry getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/**
	 * Return true if my key matches key.
	 */
	public boolean match(Position key) {
		if (key instanceof IntegerPos) {
			IntegerPos pos = (IntegerPos) key;
			//TODO why not call matchInt
			return pos.asIntegerVar().asInt32() == value().hashForEqual();
		} else {
			return false;
		}
		/*
		udanax-top.st:56599:HashIndexEntry methodsFor: 'accessing'!
		{BooleanVar} match: key {Position}
			"Return true if my key matches key."
			
			key cast: IntegerPos into: [:pos | 
					^pos asIntegerVar == self value hashForEqual]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	/**
	 * Return true if my key matches the position associated with index.
	 */
	public boolean matchInt(IntegerValue index) {
		return index.asInt32() == value().hashForEqual();
		/*
		udanax-top.st:56607:HashIndexEntry methodsFor: 'accessing'!
		{BooleanVar} matchInt: index {IntegerVar}
			"Return true if my key matches the position associated with index."
			
			^index == self value hashForEqual!
		*/
	}

	public Position position() {
		return IntegerPos.make(IntegerValue.make(value().hashForEqual()));
		/*
		udanax-top.st:56612:HashIndexEntry methodsFor: 'accessing'!
		{Position} position
			^self value hashForEqual integer!
		*/
	}

	/**
	 * Return true if my value can be replaced in place, and false if the entire entry must be
	 * replaced.
	 */
	public boolean replaceValue(Heaper newValue) {
		return newValue.hashForEqual() == value().hashForEqual() && (super.replaceValue(newValue));
		/*
		udanax-top.st:56615:HashIndexEntry methodsFor: 'accessing'!
		{BooleanVar} replaceValue: newValue {Heaper}
			"Return true if my value can be replaced in place, and false if the entire entry must be replaced."
			
			^newValue hashForEqual == self value hashForEqual and: [super replaceValue: newValue]!
		*/
	}

	public TableEntry copy() {
		return new HashIndexEntry(value());
		/*
		udanax-top.st:56622:HashIndexEntry methodsFor: 'creation'!
		{TableEntry} copy
			^ HashIndexEntry create: self value!
		*/
	}

	public HashIndexEntry(Heaper value) {
		super(value);
		/*
		udanax-top.st:56625:HashIndexEntry methodsFor: 'creation'!
		create: value {Heaper}
			super create: value!
		*/
	}

	public HashIndexEntry(TableEntry next, Heaper value) {
		super(next, value);
		/*
		udanax-top.st:56628:HashIndexEntry methodsFor: 'creation'!
		create: next {TableEntry} with: value {Heaper}
			super create: next with: value!
		*/
	}

	public HashIndexEntry(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:56633:HashIndexEntry methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:56636:HashIndexEntry methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
