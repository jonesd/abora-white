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

import java.io.PrintWriter;

import info.dgjones.abora.white.collection.tables.ActualArray;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class AscendingArrayStepper extends ArrayStepper {
	protected int lastValueInternal;
	/*
	udanax-top.st:55449:
	ArrayStepper subclass: #AscendingArrayStepper
		instanceVariableNames: 'lastValueInternal {Int32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:55453:
	(AscendingArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:55498:
	AscendingArrayStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:55501:
	(AscendingArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Stepper copy() {
		return AscendingArrayStepper.make(array(), index(), IntegerValue.make(lastValueInternal));
		/*
		udanax-top.st:55458:AscendingArrayStepper methodsFor: 'create'!
		{Stepper} copy
			^AscendingArrayStepper
				make: self array
				with: self index
				with: lastValueInternal!
		*/
	}

	protected AscendingArrayStepper(ActualArray array) {
		super(array);
		lastValueInternal = array.endOffset();
		/*
		udanax-top.st:55466:AscendingArrayStepper methodsFor: 'protected: create'!
		create: array {ActualArray}
			super create: array.
			lastValueInternal _ array endOffset!
		*/
	}

	protected AscendingArrayStepper(ActualArray array, IntegerValue index) {
		super(array, index);
		lastValueInternal = array.endOffset();
		/*
		udanax-top.st:55470:AscendingArrayStepper methodsFor: 'protected: create'!
		create: array {ActualArray} with: index {IntegerVar}
			super create: array with: index.
			lastValueInternal _ array endOffset!
		*/
	}

	protected AscendingArrayStepper(ActualArray array, IntegerValue start, IntegerValue stop) {
		super(array, start);
		lastValueInternal = stop.asInt32();
		/*
		udanax-top.st:55474:AscendingArrayStepper methodsFor: 'protected: create'!
		create: array {ActualArray} with: start {IntegerVar} with: stop {IntegerVar}
			super create: array with: start.
			lastValueInternal _ stop DOTasLong!
		*/
	}

	public Heaper fetch() {
		if (hasValue()) {
			return array().elementsArray().fetch(index().asInt32());
		} else {
			return null;
		}
		/*
		udanax-top.st:55480:AscendingArrayStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			self hasValue
				ifTrue: [^self array elementsArray fetch: self index DOTasLong]
				ifFalse: [^NULL]!
		*/
	}

	public boolean hasValue() {
		//TODO review asInt32
		return index().asInt32() <= lastValueInternal;
		/*
		udanax-top.st:55485:AscendingArrayStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^self index <= lastValueInternal!
		*/
	}

	public void step() {
		setIndex(index().asInt32() + 1);
		/*
		udanax-top.st:55488:AscendingArrayStepper methodsFor: 'operations'!
		{void} step
			self setIndex: self index DOTasLong + 1!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print(" on ");
		oo.print(array().subTableBetween(index(), IntegerValue.make(lastValueInternal)));
		/*
		udanax-top.st:55493:AscendingArrayStepper methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << ' on ' << (self array subTableBetween: self index with: lastValueInternal)!
		*/
	}

	public static TableStepper make(ActualArray array) {
		return new AscendingArrayStepper(array);
		/*
		udanax-top.st:55506:AscendingArrayStepper class methodsFor: 'create'!
		{TableStepper} make: array {ActualArray}
			^ self create: array!
		*/
	}

	public static TableStepper make(ActualArray array, IntegerValue index) {
		return new AscendingArrayStepper(array, index);
		/*
		udanax-top.st:55509:AscendingArrayStepper class methodsFor: 'create'!
		{TableStepper} make: array {ActualArray} with: index {IntegerVar}
			^ self create: array with: index!
		*/
	}

	public static TableStepper make(ActualArray array, IntegerValue start, IntegerValue stop) {
		return new AscendingArrayStepper(array, start, stop);
		/*
		udanax-top.st:55512:AscendingArrayStepper class methodsFor: 'create'!
		{TableStepper} make: array {ActualArray} with: start {IntegerVar} with: stop {IntegerVar}
			^ self create: array with: start with: stop!
		*/
	}
}
