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
package info.dgjones.abora.white.collection.steppers;

import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class OffsetScruTableStepper extends TableStepper {
	protected TableStepper myTableStepper;
	protected Dsp myDsp;
	/*
	udanax-top.st:56176:
	TableStepper subclass: #OffsetScruTableStepper
		instanceVariableNames: '
			myTableStepper {TableStepper}
			myDsp {Dsp}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:56182:
	(OffsetScruTableStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:56219:
	OffsetScruTableStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:56222:
	(OffsetScruTableStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		return myTableStepper.fetch();
		/*
		udanax-top.st:56187:OffsetScruTableStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			^ myTableStepper fetch!
		*/
	}

	public Heaper get() {
		return myTableStepper.get();
		/*
		udanax-top.st:56190:OffsetScruTableStepper methodsFor: 'operations'!
		{Heaper wimpy} get
			^ myTableStepper get!
		*/
	}

	public boolean hasValue() {
		return myTableStepper.hasValue();
		/*
		udanax-top.st:56193:OffsetScruTableStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^myTableStepper hasValue!
		*/
	}

	public void step() {
		myTableStepper.step();
		/*
		udanax-top.st:56196:OffsetScruTableStepper methodsFor: 'operations'!
		{void} step
			myTableStepper step!
		*/
	}

	public IntegerValue index() {
		return myDsp.ofInt(myTableStepper.index());
		/*
		udanax-top.st:56201:OffsetScruTableStepper methodsFor: 'special'!
		{IntegerVar} index
			^myDsp ofInt: myTableStepper index!
		*/
	}

	public Position position() {
		return myDsp.of(myTableStepper.position());
		/*
		udanax-top.st:56204:OffsetScruTableStepper methodsFor: 'special'!
		{Position} position
			^myDsp of: myTableStepper position!
		*/
	}

	public Stepper copy() {
		return new OffsetScruTableStepper(((TableStepper) myTableStepper.copy()), myDsp);
		/*
		udanax-top.st:56209:OffsetScruTableStepper methodsFor: 'create'!
		{Stepper} copy
			^ OffsetScruTableStepper create.Stepper: (myTableStepper copy cast: TableStepper) with: myDsp!
		*/
	}

	public OffsetScruTableStepper(TableStepper onStepper, Dsp aDsp) {
		super();
		myTableStepper = onStepper;
		myDsp = aDsp;
		/*
		udanax-top.st:56212:OffsetScruTableStepper methodsFor: 'create'!
		create.Stepper: onStepper {TableStepper} with: aDsp {Dsp} 
			
			super create.
			myTableStepper _ onStepper.
			myDsp _ aDsp!
		*/
	}

//	public static TableStepper create(Object aStepper, Object aDsp) {
//		return new OffsetScruTableStepper(aStepper, aDsp);
//		/*
//		udanax-top.st:56227:OffsetScruTableStepper class methodsFor: 'smalltalk: smalltalk creation'!
//		create.Stepper: aStepper with: aDsp
//			^ self new create.Stepper: aStepper with: aDsp!
//		*/
//	}
}
