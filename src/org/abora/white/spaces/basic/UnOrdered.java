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

import org.abora.white.rcvr.Rcvr;
import org.abora.white.xpp.basic.Heaper;

/**
 * A convenient superclass of all Positions which have no natural ordering.  See
 * UnOrdered::isGE for the defining property of this class.  This class should probably go
 * away and UnOrdered::isGE distributed to the subclasses.
 */
public abstract class UnOrdered extends Position {
	/*
	udanax-top.st:32953:
	Position subclass: #UnOrdered
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:32957:
	UnOrdered comment:
	'A convenient superclass of all Positions which have no natural ordering.  See UnOrdered::isGE for the defining property of this class.  This class should probably go away and UnOrdered::isGE distributed to the subclasses.'!
	*/
	/*
	udanax-top.st:32959:
	(UnOrdered getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected UnOrdered() {
		super();
	}

	protected UnOrdered(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Accessing

	public abstract XnRegion asRegion();
	/*
	udanax-top.st:32964:UnOrdered methodsFor: 'accessing'!
	{XnRegion} asRegion
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:32968:UnOrdered methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//TODOreturn Heaper.takeOop();
		/*
		udanax-top.st:32973:UnOrdered methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	/**
	 * Up in position, isGE is deferred, and isEqual is defined in terms of isEqual.
	 * Here in UnOrdered, we define isGE in terms of isEqual, so we must redefine
	 * isEqual to be deferred.
	 */
	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:32977:UnOrdered methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper} 
		"Up in position, isGE is deferred, and isEqual is defined in terms of isEqual.
		Here in UnOrdered, we define isGE in terms of isEqual, so we must redefine
		isEqual to be deferred."
		
		self subclassResponsibility!
	*/
}
