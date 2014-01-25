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
package info.dgjones.abora.white.stepper;

import info.dgjones.abora.white.cache.InstanceCache;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * This is a Stepper when you just want to step across a single item.
 */
public class ItemStepper extends Stepper {
	protected Heaper myItem;
	protected static InstanceCache SomeSteppers = InstanceCache.make(8);
	/*
	udanax-top.st:54550:
	Stepper subclass: #ItemStepper
		instanceVariableNames: 'myItem {Heaper | NULL}'
		classVariableNames: 'SomeSteppers {InstanceCache} '
		poolDictionaries: ''
		category: 'Xanadu-stepper'!
	*/
	/*
	udanax-top.st:54554:
	ItemStepper comment:
	'This is a Stepper when you just want to step across a single item.'!
	*/
	/*
	udanax-top.st:54556:
	(ItemStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:54590:
	ItemStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:54593:
	(ItemStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Stepper copy() {
		if (myItem == null) {
			return this;
		} else {
			Heaper result;
			result = SomeSteppers.fetch();
			if (result == null) {
				return new ItemStepper(myItem);
			} else {
				//TODO review
				return new ItemStepper(myItem);
			}
		}
		/*
		udanax-top.st:54561:ItemStepper methodsFor: 'create'!
		{Stepper} copy
			myItem == NULL
				ifTrue: [ ^ self ]
				ifFalse: [
					| result {Heaper} |
					result := SomeSteppers fetch.
					result == NULL
						ifTrue: [^ItemStepper create: myItem]
						ifFalse: [^(ItemStepper new.Become: result) create: myItem]]!
		*/
	}

	public ItemStepper(Heaper item) {
		super();
		myItem = item;
		/*
		udanax-top.st:54571:ItemStepper methodsFor: 'create'!
		create: item {Heaper | NULL}
			super create.
			myItem _ item!
		*/
	}

	public void destroy() {
		if (!(SomeSteppers.store(this))) {
			super.destroy();
		}
		/*
		udanax-top.st:54575:ItemStepper methodsFor: 'create'!
		{void} destroy
			(SomeSteppers store: self) ifFalse: [super destroy]!
		*/
	}

	public Heaper fetch() {
		return myItem;
		/*
		udanax-top.st:54580:ItemStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			^myItem!
		*/
	}

	public boolean hasValue() {
		return myItem != null;
		/*
		udanax-top.st:54583:ItemStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^myItem ~~ NULL!
		*/
	}

	public void step() {
		myItem = null;
		/*
		udanax-top.st:54586:ItemStepper methodsFor: 'operations'!
		{void} step
			myItem _ NULL!
		*/
	}

//	public static void initTimeNonInherited() {
//		SomeSteppers = InstanceCache.make(8);
//		/*
//		udanax-top.st:54598:ItemStepper class methodsFor: 'smalltalk: init'!
//		initTimeNonInherited
//			SomeSteppers := InstanceCache make: 8!
//		*/
//	}

//	public static void linkTimeNonInherited() {
//		SomeSteppers = null;
//		/*
//		udanax-top.st:54601:ItemStepper class methodsFor: 'smalltalk: init'!
//		linkTimeNonInherited
//			SomeSteppers := NULL!
//		*/
//	}

	public static ItemStepper make(Heaper item) {
		Heaper result;
		result = SomeSteppers.fetch();
		if (result == null) {
			return new ItemStepper(item);
		} else {
			//TODO review
			return new ItemStepper(item);
		}
		/*
		udanax-top.st:54606:ItemStepper class methodsFor: 'create'!
		{Stepper} make: item {Heaper}
			| result {Heaper} |
			result := SomeSteppers fetch.
			result == NULL
				ifTrue: [^ self create: item]
				ifFalse: [^ (self new.Become: result) create: item]!
		*/
	}
}
