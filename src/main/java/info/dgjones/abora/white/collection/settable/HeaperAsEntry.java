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
import info.dgjones.abora.white.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class HeaperAsEntry extends TableEntry {
	/*
	udanax-top.st:56639:
	TableEntry subclass: #HeaperAsEntry
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-SetTable'!
	*/
	/*
	udanax-top.st:56643:
	(HeaperAsEntry getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/**
	 * Return true if my position matches position.
	 */
	public boolean match(Position position) {
		return position().isEqual(position);
		/*
		udanax-top.st:56648:HeaperAsEntry methodsFor: 'accessing'!
		{BooleanVar} match: position {Position}
			"Return true if my position matches position."
			
			^self position isEqual: position!
		*/
	}

	public Position position() {
		return HeaperAsPosition.make(value());
		/*
		udanax-top.st:56653:HeaperAsEntry methodsFor: 'accessing'!
		{Position} position
			^HeaperAsPosition make: self value!
		*/
	}

	/**
	 * Return true if my value can be replaced in place, and false if the entire entry must be
	 * replaced.
	 */
	public boolean replaceValue(Heaper newValue) {
		return newValue.hashForEqual() == value().hashForEqual() && (super.replaceValue(newValue));
		/*
		udanax-top.st:56656:HeaperAsEntry methodsFor: 'accessing'!
		{BooleanVar} replaceValue: newValue {Heaper}
			"Return true if my value can be replaced in place, and false if the entire entry must be replaced."
			
			^newValue hashForEqual == self value hashForEqual and: [super replaceValue: newValue]!
		*/
	}

	public TableEntry copy() {
		return new HeaperAsEntry(value());
		/*
		udanax-top.st:56663:HeaperAsEntry methodsFor: 'creation'!
		{TableEntry} copy
			^ HeaperAsEntry create: self value!
		*/
	}

	protected HeaperAsEntry(Heaper value) {
		super(value);
		/*
		udanax-top.st:56666:HeaperAsEntry methodsFor: 'creation'!
		create: value {Heaper}
			super create: value!
		*/
	}

	protected HeaperAsEntry(TableEntry next, Heaper value) {
		super(next, value);
		/*
		udanax-top.st:56669:HeaperAsEntry methodsFor: 'creation'!
		create: next {TableEntry} with: value {Heaper}
			super create: next with: value!
		*/
	}

	protected HeaperAsEntry(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:56674:HeaperAsEntry methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:56677:HeaperAsEntry methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
