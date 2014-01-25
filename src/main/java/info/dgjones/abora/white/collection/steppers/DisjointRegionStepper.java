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
package info.dgjones.abora.white.collection.steppers;

import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class DisjointRegionStepper extends Stepper {
	protected XnRegion myValue;
	protected XnRegion myRegion;
	protected OrderSpec myOrder;
	/*
	udanax-top.st:53470:
	Stepper subclass: #DisjointRegionStepper
		instanceVariableNames: '
			myValue {XnRegion}
			myRegion {XnRegion}
			myOrder {OrderSpec}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:53477:
	(DisjointRegionStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:53509:
	DisjointRegionStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:53512:
	(DisjointRegionStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		return myValue;
		/*
		udanax-top.st:53482:DisjointRegionStepper methodsFor: 'accessing'!
		{Heaper wimpy} fetch
			^myValue!
		*/
	}

	public boolean hasValue() {
		return myValue != null;
		/*
		udanax-top.st:53485:DisjointRegionStepper methodsFor: 'accessing'!
		{BooleanVar} hasValue
			^myValue ~~ NULL!
		*/
	}

	public void step() {
		myValue = (XnRegion) (myRegion.simpleRegions(myOrder)).fetch();
		if (myValue == null) {
			if (!myRegion.isEmpty()) {
				throw new AboraRuntimeException(AboraRuntimeException.REGION_RETURNED_NULL_STEPPER_EVEN_THOUGH_NON_EMPTY);
			}
		} else {
			myRegion = myRegion.minus(myValue);
		}
		/*
		udanax-top.st:53488:DisjointRegionStepper methodsFor: 'accessing'!
		{void} step
			myValue _ (myRegion simpleRegions: myOrder) fetch cast: XnRegion.
			myValue == NULL
				ifTrue: [myRegion isEmpty 
							ifFalse: [Heaper BLAST: #RegionReturnedNullStepperEvenThoughNonEmpty]]
				ifFalse: [myRegion _ myRegion minus: myValue]!
		*/
	}

	public Stepper copy() {
		return DisjointRegionStepper.make((myValue.unionWith(myRegion)), myOrder);
		/*
		udanax-top.st:53497:DisjointRegionStepper methodsFor: 'instance creation'!
		{Stepper} copy
			^DisjointRegionStepper make: (myValue unionWith: myRegion) with: myOrder!
		*/
	}

	public DisjointRegionStepper(XnRegion region, OrderSpec order) {
		super();
		myValue = null;
		myRegion = region;
		myOrder = order;
		if (!myRegion.isEmpty()) {
			step();
		}
		/*
		udanax-top.st:53500:DisjointRegionStepper methodsFor: 'instance creation'!
		create: region {XnRegion} with: order {OrderSpec}
			super create.
			myValue _ NULL.
			myRegion _ region.
			myOrder _ order.
			myRegion isEmpty ifFalse:
				[self step].!
		*/
	}

	public static DisjointRegionStepper make(XnRegion region, OrderSpec order) {
		return new DisjointRegionStepper(region, order);
		/*
		udanax-top.st:53517:DisjointRegionStepper class methodsFor: 'instance creation'!
		{Stepper} make: region {XnRegion} with: order {OrderSpec}
			^DisjointRegionStepper create: region with: order!
		*/
	}
}
