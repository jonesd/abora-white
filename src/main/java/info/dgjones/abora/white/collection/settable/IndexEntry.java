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

import java.io.PrintWriter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class IndexEntry extends TableEntry {
	protected IntegerValue myIndex;
	/*
	udanax-top.st:56680:
	TableEntry subclass: #IndexEntry
		instanceVariableNames: 'myIndex {IntegerVar}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-SetTable'!
	*/
	/*
	udanax-top.st:56684:
	(IndexEntry getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	public IntegerValue index() {
		return myIndex;
		/*
		udanax-top.st:56689:IndexEntry methodsFor: 'accessing'!
		{IntegerVar} index
			^ myIndex!
		*/
	}

	/**
	 * Return true if my key matches key.
	 */
	public boolean match(Position key) {
		if (key instanceof IntegerPos) {
			IntegerPos pos = (IntegerPos) key;
			return pos.asIntegerVar() == myIndex;
		} else {
			return false;
		}
		/*
		udanax-top.st:56692:IndexEntry methodsFor: 'accessing'!
		{BooleanVar} match: key {Position}
			"Return true if my key matches key."
			
			key cast: IntegerPos into: [:pos | 
					^pos asIntegerVar == myIndex]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	/**
	 * Return true if my key matches the position associated with index.
	 */
	public boolean matchInt(IntegerValue index) {
		return index == myIndex;
		/*
		udanax-top.st:56700:IndexEntry methodsFor: 'accessing'!
		{BooleanVar} matchInt: index {IntegerVar}
			"Return true if my key matches the position associated with index."
			
			^index == myIndex!
		*/
	}

	public Position position() {
		return myIndex.integer();
		/*
		udanax-top.st:56705:IndexEntry methodsFor: 'accessing'!
		{Position} position
			^myIndex integer!
		*/
	}

	public TableEntry copy() {
		return new IndexEntry(myIndex, value());
		/*
		udanax-top.st:56710:IndexEntry methodsFor: 'creation'!
		{TableEntry} copy
			^ IndexEntry create: myIndex with:self value!
		*/
	}

	public IndexEntry(IntegerValue index, Heaper value) {
		super(value);
		myIndex = index;
		/*
		udanax-top.st:56713:IndexEntry methodsFor: 'creation'!
		create: index {IntegerVar} with: value {Heaper}
			super create: value.
			myIndex _ index!
		*/
	}

	public IndexEntry(TableEntry next, Heaper value, IntegerValue index) {
		super(next, value);
		myIndex = index;
		/*
		udanax-top.st:56717:IndexEntry methodsFor: 'creation'!
		create: next {TableEntry} with: value {Heaper} with: index {IntegerVar}
			super create: next with: value.
			myIndex _ index!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(myIndex.integer());
		oo.print(" -> ");
		oo.print(value());
		oo.print(")");
		/*
		udanax-top.st:56723:IndexEntry methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << myIndex integer << ' -> ' << self value << ')'!
		*/
	}

	public IndexEntry(Rcvr receiver) {
		super(receiver);
		myIndex = receiver.receiveIntegerVar();
		/*
		udanax-top.st:56728:IndexEntry methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myIndex _ receiver receiveIntegerVar.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendIntegerVar(myIndex);
		/*
		udanax-top.st:56732:IndexEntry methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendIntegerVar: myIndex.!
		*/
	}
}
