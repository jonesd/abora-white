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
package org.abora.white.cross;

import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.PrimIntArray;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.cross.CrossSpace;
import org.abora.white.spaces.cross.Tuple;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * A stepper for stepping through the positions in a simple cross region in order according
 * to a lexicographic composition of OrderSpecs of each of the projections of the region.
 * See CrossOrderSpec.NOT.A.TYPE
 */
public class TupleStepper extends Stepper {
	protected CrossSpace mySpace;
	protected PtrArray myVirginSteppers;
	protected PtrArray mySteppers;
	protected PrimIntArray myLexOrder;
	protected Tuple myValue;
	/*
	udanax-top.st:56321:
	Stepper subclass: #TupleStepper
		instanceVariableNames: '
			mySpace {CrossSpace}
			myVirginSteppers {PtrArray of: (Stepper of: Position)}
			mySteppers {PtrArray of: (Stepper of: Position)}
			myLexOrder {PrimIntArray}
			myValue {Tuple | NULL}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-cross'!
	*/
	/*
	udanax-top.st:56330:
	TupleStepper comment:
	'A stepper for stepping through the positions in a simple cross region in order according to a lexicographic composition of OrderSpecs of each of the projections of the region.  See CrossOrderSpec.NOT.A.TYPE '!
	*/
	/*
	udanax-top.st:56332:
	(TupleStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:56412:
	TupleStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:56415:
	(TupleStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public TupleStepper(CrossSpace space, PtrArray virginSteppers, PtrArray steppers, PrimIntArray lexOrder) {
		super();
		mySpace = space;
		myVirginSteppers = virginSteppers;
		mySteppers = steppers;
		myLexOrder = lexOrder;
		setValueFromSteppers();
		/*
		udanax-top.st:56337:TupleStepper methodsFor: 'private: creation'!
		create: space {CrossSpace} 
			with: virginSteppers {PtrArray of: (Stepper of: Position)}
			with: steppers {PtrArray of: (Stepper of: Position)}
			with: lexOrder {PrimIntArray} 
			
			super create.
			mySpace := space.
			myVirginSteppers := virginSteppers.
			mySteppers := steppers.
			myLexOrder := lexOrder.
			self setValueFromSteppers!
		*/
	}

	public void setValueFromSteppers() {
		PtrArray coords = PtrArray.make(mySteppers.count());
		for (int i = 0; i < mySteppers.count(); i++) {
			coords.store(i, ((Position) ((Stepper) (mySteppers.fetch(i))).get()));
		}
		myValue = mySpace.crossOfPositions(coords);
		/*
		udanax-top.st:56351:TupleStepper methodsFor: 'private:'!
		{void} setValueFromSteppers
			| coords {PtrArray of: Position} |
			coords := PtrArray nulls: mySteppers count.
			Int32Zero almostTo: mySteppers count do: [:i {Int32} |
				coords at: i store: (((mySteppers fetch: i) cast: Stepper) get cast: Position)].
			myValue := mySpace crossOfPositions: coords!
		*/
	}

	public Stepper copy() {
		PtrArray newSteppers;
		if (!hasValue()) {
			return Stepper.emptyStepper();
		}
		newSteppers = PtrArray.make(mySteppers.count());
		for (int i = 0; i < mySteppers.count(); i++) {
			newSteppers.store(i, ((Stepper) (mySteppers.fetch(i))).copy());
		}
		return new TupleStepper(mySpace, myVirginSteppers, newSteppers, myLexOrder);
		/*
		udanax-top.st:56361:TupleStepper methodsFor: 'operations'!
		{Stepper} copy
			
			| newSteppers {PtrArray of: (Stepper of: Position)} |
			self hasValue ifFalse:
				[^Stepper emptyStepper].
			newSteppers := PtrArray nulls: mySteppers count.
			Int32Zero almostTo: mySteppers count do: [:i {Int32} |
				newSteppers at: i store: ((mySteppers fetch: i) cast: Stepper) copy].
			^TupleStepper create: mySpace with: myVirginSteppers with: newSteppers with: myLexOrder!
		*/
	}

	public Heaper fetch() {
		return myValue;
		/*
		udanax-top.st:56371:TupleStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			^myValue!
		*/
	}

	public boolean hasValue() {
		return myValue != null;
		/*
		udanax-top.st:56375:TupleStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^myValue ~~ NULL!
		*/
	}

	public void step() {
		Stepper stomp;
		if (myValue == null) {
			return;
		}
		//TODO should this be i != 0?
		for (int i = myLexOrder.count() - 1; i > 0; i -= 1) {
			int dim;
			dim = (myLexOrder.integerAt(i)).asInt32();
			stomp = (Stepper) (mySteppers.fetch(dim));
			stomp.step();
			if (stomp.hasValue()) {
				setValueFromSteppers();
				return;
			}
			mySteppers.store(dim, ((Stepper) (myVirginSteppers.fetch(dim))).copy());
		}
		myValue = null;
		/*
		udanax-top.st:56379:TupleStepper methodsFor: 'operations'!
		{void} step
			| stomp {Stepper of: Position} |
			myValue == NULL ifTrue: [^VOID].
			myLexOrder count -1 to: Int32Zero by: -1 do: [:i {Int32} |
				| dim {Int32} |
				dim := (myLexOrder integerAt: i) DOTasLong.
				stomp := (mySteppers fetch: dim) cast: Stepper.
				stomp step.
				stomp hasValue ifTrue:
					[self setValueFromSteppers.
					^VOID].
				mySteppers at: dim store: ((myVirginSteppers fetch: dim) cast: Stepper) copy].
			myValue := NULL!
		*/
	}

	public TupleStepper(Rcvr receiver) {
		super(receiver);
		mySpace = (CrossSpace) receiver.receiveHeaper();
		myVirginSteppers = (PtrArray) receiver.receiveHeaper();
		mySteppers = (PtrArray) receiver.receiveHeaper();
		myLexOrder = (PrimIntArray) receiver.receiveHeaper();
		myValue = (Tuple) receiver.receiveHeaper();
		/*
		udanax-top.st:56395:TupleStepper methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			mySpace _ receiver receiveHeaper.
			myVirginSteppers _ receiver receiveHeaper.
			mySteppers _ receiver receiveHeaper.
			myLexOrder _ receiver receiveHeaper.
			myValue _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(mySpace);
		xmtr.sendHeaper(myVirginSteppers);
		xmtr.sendHeaper(mySteppers);
		xmtr.sendHeaper(myLexOrder);
		xmtr.sendHeaper(myValue);
		/*
		udanax-top.st:56403:TupleStepper methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: mySpace.
			xmtr sendHeaper: myVirginSteppers.
			xmtr sendHeaper: mySteppers.
			xmtr sendHeaper: myLexOrder.
			xmtr sendHeaper: myValue.!
		*/
	}

	public static Stepper make(CrossSpace space, PtrArray virginSteppers, PrimIntArray lexOrder) {
		PrimIntArray lexO;
		if (virginSteppers.count() == 0) {
			return Stepper.itemStepper((space.crossOfPositions((PtrArray.make(0)))));
		}
		PtrArray steppers = PtrArray.make(virginSteppers.count());
		for (int i = 0; i < virginSteppers.count(); i++) {
			Stepper vs;
			vs = (Stepper) (virginSteppers.fetch(i));
			if (!vs.hasValue()) {
				return Stepper.emptyStepper();
			}
			steppers.store(i, vs.copy());
		}
		if (lexOrder == null) {
			lexO = Int32Array.make(virginSteppers.count());
			for (int i = 0; i < virginSteppers.count(); i++) {
				lexO.storeInteger(i, IntegerValue.make(i));
			}
		} else {
			lexO = lexOrder;
		}
		return new TupleStepper(space, virginSteppers, steppers, lexO);
		/*
		udanax-top.st:56420:TupleStepper class methodsFor: 'pseudoconstructors'!
		{Stepper} make: space {CrossSpace} 
			with: virginSteppers {PtrArray of: (Stepper of: Position)}
			with: lexOrder {PrimIntArray default: NULL} 
			
			| steppers {PtrArray of: (Stepper of: Position)} lexO {PrimIntArray} |
			virginSteppers count = Int32Zero ifTrue:
				[^Stepper itemStepper: (space crossOfPositions: (PtrArray nulls: Int32Zero))].
			steppers := PtrArray nulls: virginSteppers count.
			Int32Zero almostTo: virginSteppers count do: [:i {Int32} |
				| vs {Stepper of: Position} |
				vs := (virginSteppers fetch: i) cast: Stepper.
				vs hasValue ifFalse:
					[^Stepper emptyStepper].
				steppers at: i store: vs copy].
				
			lexOrder == NULL ifTrue:
				[lexO := Int32Array make: virginSteppers count.
				Int32Zero almostTo: virginSteppers count do: [:i {Int32} |
					lexO at: i storeInteger: i]]
			ifFalse:
				[lexO := lexOrder].
			^self create: space with: virginSteppers with: steppers with: lexO!
		*/
	}

	public static Stepper make(CrossSpace space, PtrArray virginSteppers) {
		return make(space, virginSteppers, null);
		/*
		udanax-top.st:56445:TupleStepper class methodsFor: 'smalltalk: defaults'!
		make: space with: virginSteppers
			
			^self make: space with: virginSteppers with: NULL!
		*/
	}
}
