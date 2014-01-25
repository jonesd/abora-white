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

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class SequenceStepper extends Stepper {
	protected int myIndex;
	protected PtrArray myTransitions;
	protected int myTransitionsCount;
	/*
	udanax-top.st:55120:
	Stepper subclass: #SequenceStepper
		instanceVariableNames: '
			myIndex {Int32}
			myTransitions {PtrArray of: SequenceEdge}
			myTransitionsCount {Int32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-id'!
	*/
	/*
	udanax-top.st:55127:
	(SequenceStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		if (myIndex < myTransitionsCount) {
			return ((SequenceEdge) (myTransitions.fetch(myIndex))).sequence();
		} else {
			return null;
		}
		/*
		udanax-top.st:55132:SequenceStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			myIndex < myTransitionsCount ifTrue:
				[^((myTransitions fetch: myIndex) cast: SequenceEdge) sequence]
			ifFalse:
				[^NULL]!
		*/
	}

	public boolean hasValue() {
		return myIndex < myTransitionsCount;
		/*
		udanax-top.st:55139:SequenceStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^myIndex < myTransitionsCount!
		*/
	}

	public void step() {
		myIndex = myIndex + 2;
		/*
		udanax-top.st:55142:SequenceStepper methodsFor: 'operations'!
		{void} step
			myIndex := myIndex + 2!
		*/
	}

	public Stepper copy() {
		return new SequenceStepper(myIndex, myTransitions, myTransitionsCount);
		/*
		udanax-top.st:55147:SequenceStepper methodsFor: 'create'!
		{Stepper} copy
			
			^SequenceStepper create: myIndex with: myTransitions with: myTransitionsCount!
		*/
	}

	public SequenceStepper(PtrArray transitions, int count) {
		super();
		myIndex = 0;
		myTransitions = transitions;
		myTransitionsCount = count;
		/*
		udanax-top.st:55151:SequenceStepper methodsFor: 'create'!
		create: transitions {PtrArray of: IDEdge} with: count {Int32}
			super create.
			myIndex := Int32Zero.
			myTransitions := transitions.
			myTransitionsCount := count.!
		*/
	}

	public SequenceStepper(int index, PtrArray transitions, int count) {
		super();
		myIndex = index;
		myTransitions = transitions;
		myTransitionsCount = count;
		/*
		udanax-top.st:55158:SequenceStepper methodsFor: 'create'!
		create: index {Int32} with: transitions {PtrArray of: IDEdge} with: count {Int32}
			super create.
			myIndex := index.
			myTransitions := transitions.
			myTransitionsCount := count.!
		*/
	}
}
