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
package org.abora.white.tumbler;

import java.io.PrintWriter;

import org.abora.white.edgeregion.TransitionEdge;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.Position;
import org.abora.white.xpp.basic.Heaper;

public abstract class RealEdge extends TransitionEdge {
	protected RealPos myPos;
	/*
	udanax-top.st:63418:
	TransitionEdge subclass: #RealEdge
		instanceVariableNames: 'myPos {RealPos}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:63422:
	(RealEdge getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
	*/

	public RealPos position() {
		return myPos;
		/*
		udanax-top.st:63427:RealEdge methodsFor: 'accessing'!
		{RealPos} position
			^myPos!
		*/
	}

	public int actualHashForEqual() {
		return myPos.hashForEqual() ^ getClass().hashCode();
		/*
		udanax-top.st:63433:RealEdge methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^myPos hashForEqual bitXor: self getCategory hashForEqual!
		*/
	}

	public abstract boolean follows(Position pos);
	/*
	udanax-top.st:63437:RealEdge methodsFor: 'testing'!
	{BooleanVar} follows: pos {Position}
		
		self subclassResponsibility!
	*/

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:63441:RealEdge methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		self subclassResponsibility!
	*/

	public abstract boolean isFollowedBy(TransitionEdge next);
	/*
	udanax-top.st:63445:RealEdge methodsFor: 'testing'!
	{BooleanVar} isFollowedBy: next {TransitionEdge}
		
		self subclassResponsibility!
	*/

	public abstract boolean isGE(TransitionEdge other);
	/*
	udanax-top.st:63449:RealEdge methodsFor: 'testing'!
	{BooleanVar} isGE: other {TransitionEdge}
		
		self subclassResponsibility!
	*/

	public boolean touches(TransitionEdge other) {
		return myPos.isEqual(((RealEdge) other).position());
		/*
		udanax-top.st:63453:RealEdge methodsFor: 'testing'!
		{BooleanVar} touches: other {TransitionEdge}
			
			^myPos isEqual: (other cast: RealEdge) position!
		*/
	}

	public abstract void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious);
	/*
	udanax-top.st:63459:RealEdge methodsFor: 'printing'!
	{void} printTransitionOn: oo {ostream reference}
		with: entering {BooleanVar}
		with: touchesPrevious {BooleanVar}
		
		self subclassResponsibility!
	*/

	public RealEdge(RealPos pos) {
		super();
		myPos = pos;
		/*
		udanax-top.st:63467:RealEdge methodsFor: 'creation'!
		create: pos {RealPos}
			super create.
			myPos := pos.!
		*/
	}

	public RealEdge(Rcvr receiver) {
		super(receiver);
		myPos = (RealPos) receiver.receiveHeaper();
		/*
		udanax-top.st:63474:RealEdge methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myPos _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myPos);
		/*
		udanax-top.st:63478:RealEdge methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myPos.!
		*/
	}
}
