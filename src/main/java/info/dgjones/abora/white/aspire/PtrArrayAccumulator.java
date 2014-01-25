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
package info.dgjones.abora.white.aspire;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.steppers.Accumulator;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * To save array copies, this class will hand out its internal array if the size is right.
 * If it does so it remembers so that if new elements are introduced, a copy can be made for
 * further use.
 */
public class PtrArrayAccumulator extends Accumulator {
	protected PtrArray myValues;
	protected int myN;
	protected boolean myValuesGiven;
	/*
	udanax-top.st:12260:
	Accumulator subclass: #PtrArrayAccumulator
		instanceVariableNames: '
			myValues {PtrArray}
			myN {UInt4}
			myValuesGiven {BooleanVar}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-aspire'!
	*/
	/*
	udanax-top.st:12267:
	PtrArrayAccumulator comment:
	'To save array copies, this class will hand out its internal array if the size is right.  If it does so it remembers so that if new elements are introduced, a copy can be made for further use.'!
	*/
	/*
	udanax-top.st:12269:
	(PtrArrayAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constuctors

	public PtrArrayAccumulator() {
		this(2);
		/*
		udanax-top.st:12296:PtrArrayAccumulator methodsFor: 'create'!
		create
			super create.
			myValues := PtrArray nulls: 2.
			myN := UInt32Zero.
			myValuesGiven := false!
		*/
	}

	public PtrArrayAccumulator(int count) {
		this(PtrArray.make(count), 0);
		/*
		udanax-top.st:12302:PtrArrayAccumulator methodsFor: 'create'!
		create: count {UInt32}
			super create.
			myValues := PtrArray nulls: count.
			myN := UInt32Zero.
			myValuesGiven := false!
		*/
	}

	public PtrArrayAccumulator(PtrArray values, int n) {
		super();
		myValues = values;
		myN = n;
		myValuesGiven = false;
		/*
		udanax-top.st:12308:PtrArrayAccumulator methodsFor: 'create'!
		create: values {PtrArray} with: n {UInt32}
			super create.
			myValues := values.
			myN := n.
			myValuesGiven := false!
		*/
	}

	public Accumulator copy() {
		return new PtrArrayAccumulator(((PtrArray) myValues.copy()), myN);
		/*
		udanax-top.st:12274:PtrArrayAccumulator methodsFor: 'operations'!
		{Accumulator} copy
			^PtrArrayAccumulator create: (myValues copy cast: PtrArray) with: myN!
		*/
	}

	public void step(Heaper x) {
		if (!(myN + 1 < myValues.count())) {
			myValues = (PtrArray) (myValues.copyGrow(myValues.count() + 1));
		}
		myValues.store(myN, x);
		myN = myN + 1;
		/*
		udanax-top.st:12278:PtrArrayAccumulator methodsFor: 'operations'!
		{void} step: x {Heaper}
			myN + 1 < myValues count ifFalse:
				[myValues := (myValues copyGrow: myValues count+1) cast: PtrArray].
			myValues at: myN store: x.
			myN := myN + 1.!
		*/
	}

	public Heaper value() {
		if (myValues.count() == myN) {
			myValuesGiven = true;
			return myValues;
		} else {
			return myValues.copy(myN);
		}
		/*
		udanax-top.st:12285:PtrArrayAccumulator methodsFor: 'operations'!
		{Heaper} value
			myValues count == myN
				ifTrue: [
					myValuesGiven := true.
					^ myValues]
				ifFalse: [
					^myValues copy: myN]!
		*/
	}
}
