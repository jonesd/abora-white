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
package org.abora.white.stepper;

import org.abora.white.cache.InstanceCache;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.xpp.basic.Heaper;

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
