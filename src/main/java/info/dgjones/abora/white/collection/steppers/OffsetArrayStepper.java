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

public class OffsetArrayStepper extends TableStepper {
	protected TableStepper myArrayStepper;
	protected Dsp myDsp;
	/*
	udanax-top.st:56112:
	TableStepper subclass: #OffsetArrayStepper
		instanceVariableNames: '
			myArrayStepper {TableStepper}
			myDsp {Dsp}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:56118:
	(OffsetArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:56157:
	OffsetArrayStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:56160:
	(OffsetArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		return myArrayStepper.fetch();
		/*
		udanax-top.st:56123:OffsetArrayStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			^ myArrayStepper fetch!
		*/
	}

	public Heaper get() {
		return myArrayStepper.get();
		/*
		udanax-top.st:56126:OffsetArrayStepper methodsFor: 'operations'!
		{Heaper wimpy} get
			^ myArrayStepper get!
		*/
	}

	public boolean hasValue() {
		return myArrayStepper.hasValue();
		/*
		udanax-top.st:56129:OffsetArrayStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^myArrayStepper hasValue!
		*/
	}

	public void step() {
		myArrayStepper.step();
		/*
		udanax-top.st:56132:OffsetArrayStepper methodsFor: 'operations'!
		{void} step
			myArrayStepper step!
		*/
	}

	public IntegerValue index() {
		return myDsp.ofInt(myArrayStepper.index());
		/*
		udanax-top.st:56137:OffsetArrayStepper methodsFor: 'special'!
		{IntegerVar} index
			^myDsp ofInt: myArrayStepper index!
		*/
	}

	public Position position() {
		return myDsp.of(myArrayStepper.position());
		/*
		udanax-top.st:56140:OffsetArrayStepper methodsFor: 'special'!
		{Position} position
			^myDsp of: myArrayStepper position!
		*/
	}

	public OffsetArrayStepper(TableStepper onStepper, Dsp aDsp) {
		super();
		myArrayStepper = onStepper;
		myDsp = aDsp;
		/*
		udanax-top.st:56145:OffsetArrayStepper methodsFor: 'protected: create'!
		create.Stepper: onStepper {TableStepper} with: aDsp {Dsp} 
			
			super create.
			myArrayStepper _ onStepper.
			myDsp _ aDsp!
		*/
	}

	public Stepper copy() {
		return OffsetArrayStepper.make(((TableStepper) myArrayStepper.copy()), myDsp);
		/*
		udanax-top.st:56153:OffsetArrayStepper methodsFor: 'create'!
		{Stepper} copy
			^ OffsetArrayStepper make: (myArrayStepper copy cast: TableStepper) with: myDsp!
		*/
	}

//	public static TableStepper create(Object aStepper) {
//		return new OffsetArrayStepper(aStepper);
//		/*
//		udanax-top.st:56165:OffsetArrayStepper class methodsFor: 'smalltalk: creation'!
//		create.Stepper: aStepper
//			^ self new create.Stepper: aStepper!
//		*/
//	}

//	public static TableStepper create(Object aStepper, Object aDsp) {
//		return new OffsetArrayStepper(aStepper, aDsp);
//		/*
//		udanax-top.st:56168:OffsetArrayStepper class methodsFor: 'smalltalk: creation'!
//		create.Stepper: aStepper with: aDsp
//			^ self new create.Stepper: aStepper with: aDsp!
//		*/
//	}

	public static TableStepper make(TableStepper arrayStepper, Dsp aDsp) {
		return new OffsetArrayStepper(arrayStepper, aDsp);
		/*
		udanax-top.st:56173:OffsetArrayStepper class methodsFor: 'create'!
		{TableStepper} make: arrayStepper {TableStepper} with: aDsp {Dsp} 
			^self create.Stepper: arrayStepper with: aDsp!
		*/
	}
}
