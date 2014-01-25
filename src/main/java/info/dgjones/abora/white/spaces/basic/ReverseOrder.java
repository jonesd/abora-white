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
package info.dgjones.abora.white.spaces.basic;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class ReverseOrder extends OrderSpec {
	protected OrderSpec myOrder;
	/*
	udanax-top.st:31039:
	OrderSpec subclass: #ReverseOrder
		instanceVariableNames: 'myOrder {OrderSpec}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:31043:
	(ReverseOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:31105:
	ReverseOrder class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:31108:
	(ReverseOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	public ReverseOrder(OrderSpec order) {
		super();
		if (order == null) {
			throw new IllegalArgumentException();
		}
		myOrder = order;
		/*
		udanax-top.st:31089:ReverseOrder methodsFor: 'private: creation'!
		create: order {OrderSpec}
			super create.
			myOrder := order!
		*/
	}

	public ReverseOrder(Rcvr receiver) {
		super(receiver);
		myOrder = (OrderSpec) receiver.receiveHeaper();
		/*
		udanax-top.st:31096:ReverseOrder methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myOrder _ receiver receiveHeaper.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static OrderSpec make(OrderSpec order) {
		return new ReverseOrder(order);
		/*
		udanax-top.st:31113:ReverseOrder class methodsFor: 'pseudoconstructors'!
		{OrderSpec} make: order {OrderSpec}
			^self create: order!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public CoordinateSpace coordinateSpace() {
		return myOrder.coordinateSpace();
		/*
		udanax-top.st:31048:ReverseOrder methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^myOrder coordinateSpace!
		*/
	}

	public OrderSpec reversed() {
		return myOrder;
		/*
		udanax-top.st:31052:ReverseOrder methodsFor: 'accessing'!
		{OrderSpec} reversed
			
			^myOrder!
		*/
	}

	/////////////////////////////////////////////
	// Testing

	public int actualHashForEqual() {
		return myOrder.hashForEqual() ^ -1;
		/*
		udanax-top.st:31058:ReverseOrder methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^myOrder hashForEqual bitXor: -1!
		*/
	}

	public boolean follows(Position x, Position y) {
		return myOrder.follows(y, x);
		/*
		udanax-top.st:31062:ReverseOrder methodsFor: 'testing'!
		{BooleanVar} follows: x {Position} with: y {Position}
			
			^myOrder follows: y with: x!
		*/
	}

	public boolean followsInt(IntegerValue x, IntegerValue y) {
		return myOrder.followsInt(y, x);
		/*
		udanax-top.st:31066:ReverseOrder methodsFor: 'testing'!
		{BooleanVar} followsInt: x {IntegerVar} with: y {IntegerVar}
			^myOrder followsInt: y with: x!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof OrderSpec) {
			OrderSpec orderSpec = (OrderSpec) other;
			return myOrder.isEqual(orderSpec.reversed());
		} else {
			return false;
		}

		/*
		udanax-top.st:31070:ReverseOrder methodsFor: 'testing'!
		{BooleanVar} isEqual: other{Heaper}
			other cast: OrderSpec into: [:os |
					^myOrder isEqual: os reversed]
				others: [^false].
			^false "fodder"!
		*/
	}

	public boolean isFullOrder(XnRegion keys) {
		return myOrder.isFullOrder(keys);
		/*
		udanax-top.st:31077:ReverseOrder methodsFor: 'testing'!
		{BooleanVar} isFullOrder: keys {XnRegion default: NULL}
			^myOrder isFullOrder: keys!
		*/
	}

	/**
	 * Return true if some position in before is less than or equal to all positions in after.
	 */
	public boolean preceeds(XnRegion before, XnRegion after) {
		throw new UnsupportedOperationException();
//		unimplemented();
		/*
		udanax-top.st:31081:ReverseOrder methodsFor: 'testing'!
		{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
			"Return true if some position in before is less than or equal to all positions in after."
			
			self unimplemented.
			^false!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myOrder);
		/*
		udanax-top.st:31100:ReverseOrder methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myOrder.!
		*/
	}
}
