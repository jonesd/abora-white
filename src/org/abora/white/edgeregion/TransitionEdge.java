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
package org.abora.white.edgeregion;

import java.io.PrintWriter;

import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.Position;
import org.abora.white.xpp.basic.Heaper;

/**
 * Clients of EdgeManager define concrete subclasses of this, which are then used by the
 * EdgeManager code
 */
public abstract class TransitionEdge extends Heaper {
	/*
	udanax-top.st:63348:
	Heaper subclass: #TransitionEdge
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-EdgeRegion'!
	*/
	/*
	udanax-top.st:63352:
	TransitionEdge comment:
	'Clients of EdgeManager define concrete subclasses of this, which are then used by the EdgeManager code'!
	*/
	/*
	udanax-top.st:63354:
	(TransitionEdge getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected TransitionEdge() {
		super();
	}

	public TransitionEdge ceiling(TransitionEdge other) {
		if (other.isGE(this)) {
			return other;
		} else {
			return this;
		}
		/*
		udanax-top.st:63359:TransitionEdge methodsFor: 'accessing'!
		{TransitionEdge} ceiling: other {TransitionEdge}
			(other isGE: self)
				ifTrue: [^other]
				ifFalse: [^self]!
		*/
	}

	public TransitionEdge floor(TransitionEdge other) {
		if (isGE(other)) {
			return other;
		} else {
			return this;
		}
		/*
		udanax-top.st:63365:TransitionEdge methodsFor: 'accessing'!
		{TransitionEdge} floor: other {TransitionEdge}
			(self isGE: other)
				ifTrue: [^other]
				ifFalse: [^self]!
		*/
	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
//		return Heaper.takeOop();
		/*
		udanax-top.st:63373:TransitionEdge methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	/**
	 * Whether the position is strictly less than this edge
	 */
	public abstract boolean follows(Position pos);
		/*
		udanax-top.st:63377:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} follows: pos {Position}
			"Whether the position is strictly less than this edge"
			
			self subclassResponsibility!
		*/
	

	public abstract boolean isEqual(Heaper other);
		/*
		udanax-top.st:63382:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			self subclassResponsibility!
		*/
	

	/**
	 * Whether there is precisely one position between this edge and the next one
	 */
	public abstract boolean isFollowedBy(TransitionEdge next);
		/*
		udanax-top.st:63386:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} isFollowedBy: next {TransitionEdge}
			"Whether there is precisely one position between this edge and the next one"
			
			self subclassResponsibility!
		*/
	

	/**
	 * Defines a full ordering among all edges in a given CoordinateSpace
	 */
	public abstract boolean isGE(TransitionEdge other);
		/*
		udanax-top.st:63391:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} isGE: other {TransitionEdge}
			"Defines a full ordering among all edges in a given CoordinateSpace"
			
			self subclassResponsibility!
		*/
	

	/**
	 * Whether this edge touches the same position the other does
	 */
	public abstract boolean touches(TransitionEdge other);
		/*
		udanax-top.st:63396:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} touches: other {TransitionEdge}
			"Whether this edge touches the same position the other does"
			
			self subclassResponsibility!
		*/
	

	/**
	 * Print a description of this transition
	 */
	public abstract void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious);
		/*
		udanax-top.st:63403:TransitionEdge methodsFor: 'printing'!
		{void} printTransitionOn: oo {ostream reference}
			with: entering {BooleanVar}
			with: touchesPrevious {BooleanVar}
			"Print a description of this transition"
			
			self subclassResponsibility!
		*/
	

	public TransitionEdge(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:63412:TransitionEdge methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:63415:TransitionEdge methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
