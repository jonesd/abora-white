/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.white.settab;

import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.xpp.basic.Heaper;

public class SetTableStepper extends Stepper {
	protected PtrArray myPtrs;
	protected int myIndex;
	protected static PtrArray AnArray;
	protected static SetTableStepper AStepper;
	/*
	udanax-top.st:55165:
	Stepper subclass: #SetTableStepper
		instanceVariableNames: '
			myPtrs {PtrArray}
			myIndex {Int32}'
		classVariableNames: '
			AnArray {PtrArray} 
			AStepper {SetTableStepper} '
		poolDictionaries: ''
		category: 'Xanadu-settab'!
	*/
	/*
	udanax-top.st:55173:
	(SetTableStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/
	/*
	udanax-top.st:55223:
	SetTableStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:55226:
	(SetTableStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/

	public Heaper fetch() {
		if (myIndex < myPtrs.count()) {
			return myPtrs.fetch(myIndex);
		} else {
			return null;
		}
		/*
		udanax-top.st:55178:SetTableStepper methodsFor: 'accessing'!
		{Heaper} fetch
			myIndex < myPtrs count
				ifTrue: [^ myPtrs fetch: myIndex]
				ifFalse: [^ NULL]!
		*/
	}

	public boolean hasValue() {
		return myIndex < myPtrs.count();
		/*
		udanax-top.st:55183:SetTableStepper methodsFor: 'accessing'!
		{BooleanVar} hasValue
			^ myIndex < myPtrs count!
		*/
	}

	public void step() {
		myIndex = myIndex + 1;
		/*
		udanax-top.st:55186:SetTableStepper methodsFor: 'accessing'!
		{void} step
			myIndex := myIndex + 1!
		*/
	}

	public Stepper copy() {
		return new SetTableStepper(((PtrArray) myPtrs.copy()), myIndex);
		/*
		udanax-top.st:55191:SetTableStepper methodsFor: 'create'!
		{Stepper} copy
			^ SetTableStepper create: (myPtrs copy cast: PtrArray) with: myIndex!
		*/
	}

	public void destroy() {
		if (AStepper == null) {
			myPtrs.zeroElements();
			AnArray = (PtrArray) myPtrs;
			AStepper = this;
			destruct();
		} else {
			super.destroy();
		}
		/*
		udanax-top.st:55194:SetTableStepper methodsFor: 'create'!
		{void} destroy
			AStepper == NULL
				ifTrue: [
					myPtrs storeAll.
					AnArray := myPtrs cast: PtrArray.
					AStepper := self.
					self destruct]
				ifFalse: [
					super destroy].!
		*/
	}

	public SetTableStepper(PtrArray array) {
		super();
		myPtrs = array;
		myIndex = 0;
		/*
		udanax-top.st:55206:SetTableStepper methodsFor: 'protected: create'!
		create: array {PtrArray}
			super create.
			myPtrs := array.
			myIndex := Int32Zero.!
		*/
	}

	public SetTableStepper(PtrArray array, int index) {
		super();
		myPtrs = array;
		myIndex = index;
		/*
		udanax-top.st:55211:SetTableStepper methodsFor: 'protected: create'!
		create: array {PtrArray} with: index {Int32}
			super create.
			myPtrs := array.
			myIndex := index.!
		*/
	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
//		return asOop();
		/*
		udanax-top.st:55218:SetTableStepper methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Object other) {
		return this == other;
		/*
		udanax-top.st:55220:SetTableStepper methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}

	public static void linkTimeNonInherited() {
		AnArray = null;
		AStepper = null;
		/*
		udanax-top.st:55231:SetTableStepper class methodsFor: 'smalltalk: init'!
		linkTimeNonInherited
			AnArray := NULL.
			AStepper := NULL.!
		*/
	}

	public static SetTableStepper make(PtrArray array) {
		if (AStepper != null) {
			SetTableStepper result;
			//TODO review new
			result = new SetTableStepper(array);
			AStepper = null;
			return result;
		} else {
			return new SetTableStepper(array);
		}
		/*
		udanax-top.st:55237:SetTableStepper class methodsFor: 'create'!
		make: array {PtrArray}
			AStepper ~~ NULL
				ifTrue: [
					| result {SetTableStepper} |
					result := ((SetTableStepper new.Become: AStepper) create: array) .
					AStepper := NULL.
					^ result]
				ifFalse: [
					^ SetTableStepper create: array]!
		*/
	}

	public static PtrArray array() {
		if (AnArray != null) {
			PtrArray result;
			result = AnArray;
			AnArray = null;
			return result;
		} else {
			return PtrArray.make(16);
		}
		/*
		udanax-top.st:55249:SetTableStepper class methodsFor: 'accessing'!
		{PtrArray} array
			AnArray ~~ NULL
				ifTrue: [
					| result {PtrArray} |
					result := AnArray.
					AnArray := NULL.
					^ result]
				ifFalse: [
					^ PtrArray nulls: 16]!
		*/
	}
}
