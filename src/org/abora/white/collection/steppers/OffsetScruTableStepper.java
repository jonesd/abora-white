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

import org.abora.white.spaces.basic.Dsp;
import org.abora.white.spaces.basic.Position;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

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
