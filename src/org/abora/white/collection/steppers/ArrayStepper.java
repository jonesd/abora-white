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
package org.abora.white.collection.steppers;

import org.abora.white.collection.tables.ActualArray;
import org.abora.white.collection.tables.MuArray;
import org.abora.white.spaces.basic.Position;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public abstract class ArrayStepper extends TableStepper {
	protected ActualArray arrayInternal;
	protected int indexInternal;
	/*
	udanax-top.st:55393:
	TableStepper subclass: #ArrayStepper
		instanceVariableNames: '
			arrayInternal {ActualArray}
			indexInternal {Int32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:55399:
	(ArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
	*/

	public abstract Heaper fetch();
		/*
		udanax-top.st:55404:ArrayStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			self subclassResponsibility!
		*/
	

	public Heaper get() {
		return arrayInternal.intGet(IntegerValue.make(indexInternal));
		/*
		udanax-top.st:55407:ArrayStepper methodsFor: 'operations'!
		{Heaper wimpy} get
			^arrayInternal intGet: indexInternal!
		*/
	}

	public abstract boolean hasValue();
		/*
		udanax-top.st:55410:ArrayStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			self subclassResponsibility!
		*/
	

	public abstract void step();
		/*
		udanax-top.st:55413:ArrayStepper methodsFor: 'operations'!
		{void} step
			self subclassResponsibility!
		*/
	

	public IntegerValue index() {
		//TODO wrapped in integervalue make
		return IntegerValue.make(indexInternal);
		/*
		udanax-top.st:55418:ArrayStepper methodsFor: 'special'!
		{IntegerVar} index
			^indexInternal!
		*/
	}

	public Position position() {
		return index().integer();
		/*
		udanax-top.st:55421:ArrayStepper methodsFor: 'special'!
		{Position} position
			^indexInternal integer!
		*/
	}

	public ActualArray array() {
		return arrayInternal;
		/*
		udanax-top.st:55426:ArrayStepper methodsFor: 'protected: accessing'!
		{ActualArray} array
			^arrayInternal!
		*/
	}

	public void setIndex(int i) {
		indexInternal = i;
		/*
		udanax-top.st:55429:ArrayStepper methodsFor: 'protected: accessing'!
		{void} setIndex: i {Int32}
			indexInternal _ i!
		*/
	}

	public abstract Stepper copy();
		/*
		udanax-top.st:55434:ArrayStepper methodsFor: 'create'!
		{Stepper} copy
			self subclassResponsibility!
		*/
	

	protected ArrayStepper(MuArray array) {
		super();
		arrayInternal = (ActualArray) array.copy();
		indexInternal = 0;
		/*
		udanax-top.st:55439:ArrayStepper methodsFor: 'protected: create'!
		create: array {MuArray}
			super create.
			arrayInternal _ array copy cast: ActualArray.
			indexInternal _ Int32Zero!
		*/
	}

	protected ArrayStepper(MuArray array, IntegerValue index) {
		super();
		arrayInternal = (ActualArray) array.copy();
		indexInternal = index.asInt32();
		/*
		udanax-top.st:55444:ArrayStepper methodsFor: 'protected: create'!
		create: array {MuArray} with: index {IntegerVar}
			super create.
			arrayInternal _ array copy cast: ActualArray.
			indexInternal _ index DOTasLong!
		*/
	}
}
