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
package org.abora.white.spaces.basic;

import org.abora.white.arrange.Arrangement;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.integers.IntegerArrangement;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.spaces.integers.IntegerSpace;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class IntegerUpOrder extends OrderSpec {
	/*
	udanax-top.st:30887:
	OrderSpec subclass: #IntegerUpOrder
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:30891:
	(IntegerUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:30955:
	IntegerUpOrder class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:30958:
	(IntegerUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
		//		TODO return CAT.hashForEqual() + 1;
		/*
		udanax-top.st:30896:IntegerUpOrder methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^#cat.U.IntegerUpOrder hashForEqual + 1!
		*/
	}

	public boolean follows(Position x, Position y) {
		return ((IntegerPos) x).asIntegerVar().isGE(((IntegerPos) y).asIntegerVar());
		/*
		udanax-top.st:30900:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} follows: x {Position} with: y {Position}
			^(x cast: IntegerPos) asIntegerVar >= (y cast: IntegerPos) asIntegerVar!
		*/
	}

	/**
	 * See discussion in XuInteger class comment about boxed vs unboxed integers
	 */
	public boolean followsInt(IntegerValue x, IntegerValue y) {
		return x.isGE(y);
		/*
		udanax-top.st:30904:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} followsInt: x {IntegerVar} with: y {IntegerVar}
			"See discussion in XuInteger class comment about boxed vs unboxed integers"
			^ x >= y!
		*/
	}

	public boolean isEqual(Heaper other) {
		return other instanceof IntegerUpOrder;
		/*
		udanax-top.st:30909:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			^other isKindOf: IntegerUpOrder!
		*/
	}

	public boolean isFullOrder(XnRegion keys) {
		return true;
		/*
		udanax-top.st:30913:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} isFullOrder: keys {XnRegion unused default: NULL}
			
			^true!
		*/
	}

	/**
	 * Return true if some position in before is less than or equal to all positions in after.
	 */
	public boolean preceeds(XnRegion before, XnRegion after) {
		IntegerRegion first;
		IntegerRegion second;
		first = (IntegerRegion) before;
		second = (IntegerRegion) after;
		if (!first.isBoundedBelow()) {
			return true;
		}
		if (!second.isBoundedBelow()) {
			return false;
		}
		return first.start().isLE(second.start());
		/*
		udanax-top.st:30917:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
			"Return true if some position in before is less than or equal to all positions in after."
			
			| first {IntegerRegion} second {IntegerRegion} |
			first _ before cast: IntegerRegion.
			second _ after cast: IntegerRegion.
			first isBoundedBelow ifFalse: [^true].
			second isBoundedBelow ifFalse: [^false].
			^first start <= second start!
		*/
	}

	public Arrangement arrange(XnRegion region) {
		return IntegerArrangement.make(region, this);
		/*
		udanax-top.st:30929:IntegerUpOrder methodsFor: 'accessing'!
		{Arrangement} arrange: region {XnRegion}
			^IntegerArrangement make: region with: self.!
		*/
	}

	/**
	 * Return the first n positions in the region according to my ordering.
	 */
	public XnRegion chooseMany(XnRegion region, IntegerValue n) {
		return (arrange(region)).keysOf(0, n.asInt32());
		/*
		udanax-top.st:30932:IntegerUpOrder methodsFor: 'accessing'!
		{XnRegion} chooseMany: region {XnRegion} with: n {IntegerVar}
			"Return the first n positions in the region according to my ordering."
			
			^(self arrange: region) keysOf: Int32Zero with: n DOTasLong!
		*/
	}

	/**
	 * Return the first position in the region according to my ordering.
	 */
	public Position chooseOne(XnRegion region) {
		return IntegerPos.make(((IntegerRegion) region).start());
		/*
		udanax-top.st:30937:IntegerUpOrder methodsFor: 'accessing'!
		{Position} chooseOne: region {XnRegion}
			"Return the first position in the region according to my ordering."
			
			^IntegerPos make: (region cast: IntegerRegion) start!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:30942:IntegerUpOrder methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^IntegerSpace make!
		*/
	}

	protected IntegerUpOrder() {
		super();
	}

	protected IntegerUpOrder(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:30948:IntegerUpOrder methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:30951:IntegerUpOrder methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static OrderSpec make() {
		return new IntegerUpOrder();
		/*
		udanax-top.st:30963:IntegerUpOrder class methodsFor: 'pseudoconstructors'!
		{OrderSpec} make
			^self create!
		*/
	}
}
