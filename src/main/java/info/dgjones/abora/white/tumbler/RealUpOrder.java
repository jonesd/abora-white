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
package info.dgjones.abora.white.tumbler;

import info.dgjones.abora.white.arrange.Arrangement;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class RealUpOrder extends OrderSpec {
	/*
	udanax-top.st:30967:
	OrderSpec subclass: #RealUpOrder
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:30971:
	(RealUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:31027:
	RealUpOrder class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:31030:
	(RealUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected RealUpOrder() {
		super();
	}

	protected RealUpOrder(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:31020:RealUpOrder methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static OrderSpec make() {
		return new RealUpOrder();
		/*
		udanax-top.st:31035:RealUpOrder class methodsFor: 'creation'!
		{OrderSpec} make
			^self create!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public Arrangement arrange(XnRegion region) {
		Stepper stepper;
		PtrArray array;
		if (!region.isFinite()) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
		}
		stepper = ((RealRegion) region).stepper();
		array = (PtrArray) stepper.stepMany();
		if (!stepper.atEnd()) {
			throw new UnsupportedOperationException();
//			unimplemented();
		}
		return ExplicitArrangement.make(array);
		/*
		udanax-top.st:30976:RealUpOrder methodsFor: 'accessing'!
		{Arrangement} arrange: region {XnRegion}
			| stepper {Stepper} array {PtrArray} |
			region isFinite ifFalse: [Heaper BLAST: #MustBeFinite].
			stepper := (region cast: RealRegion) stepper.
			array := stepper stepMany cast: PtrArray.
			stepper atEnd ifFalse: [self unimplemented].
			^ExplicitArrangement make: array!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return RealSpace.make();
		/*
		udanax-top.st:30985:RealUpOrder methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^RealSpace make!
		*/
	}

	/////////////////////////////////////////////
	// Testing

	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
//		return CAT.hashForEqual() + 1;
		/*
		udanax-top.st:30991:RealUpOrder methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^#cat.U.RealUpOrder hashForEqual + 1!
		*/
	}

	public boolean follows(Position x, Position y) {
//		TODO MarkM.thingToDo(); /* 128 bit values */
		return ((RealPos) x).asIEEE64() >= ((RealPos) y).asIEEE64();
		/*
		udanax-top.st:30995:RealUpOrder methodsFor: 'testing'!
		{BooleanVar} follows: x {Position} with: y {Position}
			
			MarkM thingToDo. "128 bit values"
			^(x cast: RealPos) asIEEE64 >= (y cast: RealPos) asIEEE64!
		*/
	}

	public boolean isEqual(Heaper other) {
		return other instanceof RealUpOrder;
		/*
		udanax-top.st:31000:RealUpOrder methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			^other isKindOf: RealUpOrder!
		*/
	}

	public boolean isFullOrder(XnRegion keys) {
		return true;
		/*
		udanax-top.st:31004:RealUpOrder methodsFor: 'testing'!
		{BooleanVar} isFullOrder: keys {XnRegion default: NULL}
			
			^true!
		*/
	}

	public boolean preceeds(XnRegion before, XnRegion after) {
		if (before instanceof RealRegion) {
			RealRegion br = (RealRegion) before;
			if (!br.isBoundedBelow()) {
				return true;
			}
			if (after instanceof RealRegion) {
				RealRegion ar = (RealRegion) after;
				return !ar.isBoundedBelow() && (follows(ar.lowerBound(), br.lowerBound()));
			}
		}
		return false
		/* fodder */;
		/*
		udanax-top.st:31008:RealUpOrder methodsFor: 'testing'!
		{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
			before cast: RealRegion into: [ :br |
				br isBoundedBelow ifFalse:
					[^true].
				after cast: RealRegion into: [ :ar |
					^ar isBoundedBelow not
						and: [self follows: ar lowerBound with: br lowerBound]]].
			^false "fodder"!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:31023:RealUpOrder methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
