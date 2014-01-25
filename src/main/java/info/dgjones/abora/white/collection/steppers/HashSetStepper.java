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

import info.dgjones.abora.white.cache.InstanceCache;
import info.dgjones.abora.white.collection.arrays.SharedPtrArray;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class HashSetStepper extends Stepper {
	protected SharedPtrArray myElements;
	protected int myCurrent;
	protected static InstanceCache SomeSteppers = InstanceCache.make(16);
	/*
	udanax-top.st:54158:
	Stepper subclass: #HashSetStepper
		instanceVariableNames: '
			myElements {SharedPtrArray}
			myCurrent {Int32}'
		classVariableNames: 'SomeSteppers {InstanceCache} '
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:54164:
	(HashSetStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:54222:
	HashSetStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:54225:
	(HashSetStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		if (myElements != null && (myCurrent < myElements.count())) {
			return myElements.fetch(myCurrent);
		} else {
			return null;
		}
		/*
		udanax-top.st:54169:HashSetStepper methodsFor: 'accessing'!
		{Heaper wimpy} fetch
			(myElements ~~ NULL and: [myCurrent < myElements count])
				ifTrue: [^myElements fetch: myCurrent]
				ifFalse: [^NULL]!
		*/
	}

	public boolean hasValue() {
		return myElements != null && (myCurrent < myElements.count());
		/*
		udanax-top.st:54174:HashSetStepper methodsFor: 'accessing'!
		{BooleanVar} hasValue
			^myElements ~~ NULL and: [myCurrent < myElements count]!
		*/
	}

	public void step() {
		myCurrent = myCurrent + 1;
		verifyEntry();
		/*
		udanax-top.st:54177:HashSetStepper methodsFor: 'accessing'!
		{void} step
			myCurrent _ myCurrent + 1.
			self verifyEntry!
		*/
	}

	public void destruct() {
		if (myElements != null) {
			myElements.shareLess();
		}
		super.destruct();
		/*
		udanax-top.st:54183:HashSetStepper methodsFor: 'protected: destruct'!
		{void} destruct
			myElements ~~ NULL ifTrue: [
				myElements shareLess].
			super destruct!
		*/
	}

	public HashSetStepper(SharedPtrArray elements, int current) {
		super();
		myElements = elements;
		myElements.shareMore();
		myCurrent = current;
		verifyEntry();
		/*
		udanax-top.st:54190:HashSetStepper methodsFor: 'protected: creation'!
		create: elements {SharedPtrArray} with: current {Int32}
			super create.
			myElements _ elements.
			myElements shareMore.
			myCurrent _ current.
			self verifyEntry!
		*/
	}

	public Stepper copy() {
		Heaper result = SomeSteppers.fetch();
		if (result == null) {
			return new HashSetStepper(myElements, myCurrent);
		} else {
			//TODO review
			return new HashSetStepper(myElements, myCurrent);
		}
		/*
		udanax-top.st:54199:HashSetStepper methodsFor: 'creation'!
		{Stepper} copy
			| result {Heaper} |
			result := SomeSteppers fetch.
			result == NULL
				ifTrue: [^ HashSetStepper create: myElements with: myCurrent]
				ifFalse: [^ (HashSetStepper new.Become: result) create: myElements with: myCurrent]!
		*/
	}

	public void destroy() {
		if (!(SomeSteppers.store(this))) {
			super.destroy();
		}
		/*
		udanax-top.st:54206:HashSetStepper methodsFor: 'creation'!
		{void} destroy
			(SomeSteppers store: self) ifFalse:
				[super destroy]!
		*/
	}

	public void verifyEntry() {
		if (myElements != null) {
			if (myCurrent < myElements.count()) {
				myCurrent = myElements.indexPast(null, myCurrent);
				if (myCurrent == -1) {
					myCurrent = myElements.count();
				}
			}
			if (!hasValue()) {
				myElements.shareLess();
				myElements = null;
			}
		}
		/*
		udanax-top.st:54212:HashSetStepper methodsFor: 'private:'!
		{void} verifyEntry
			myElements ~~ NULL ifTrue: [
				myCurrent < myElements count ifTrue: [
					myCurrent := myElements indexPast: NULL with: myCurrent.
					myCurrent == -1 ifTrue: [myCurrent := myElements count]].
				self hasValue ifFalse: [
					myElements shareLess.
					myElements := NULL]].!
		*/
	}

	public static HashSetStepper make(SharedPtrArray elements) {
		Heaper result = SomeSteppers.fetch();
		if (result == null) {
			return new HashSetStepper(elements, 0);
		} else {
			//TODO review
			return new HashSetStepper(elements, 0);
		}
		/*
		udanax-top.st:54230:HashSetStepper class methodsFor: 'pseudo constructors'!
		{Stepper} make: elements {SharedPtrArray}
			| result {Heaper} |
			result := SomeSteppers fetch.
			result == NULL
				ifTrue: [^ self create: elements with: Int32Zero]
				ifFalse: [^ (self new.Become: result) create: elements with: Int32Zero]!
		*/
	}

//	public static void initTimeNonInherited() {
//		SomeSteppers = InstanceCache.make(16);
//		/*
//		udanax-top.st:54239:HashSetStepper class methodsFor: 'smalltalk: init'!
//		initTimeNonInherited
//			SomeSteppers := InstanceCache make: 16.!
//		*/
//	}

//	public static void linkTimeNonInherited() {
//		SomeSteppers = null;
//		/*
//		udanax-top.st:54242:HashSetStepper class methodsFor: 'smalltalk: init'!
//		linkTimeNonInherited
//			SomeSteppers := NULL!
//		*/
//	}
}
