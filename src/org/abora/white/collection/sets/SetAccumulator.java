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
package org.abora.white.collection.sets;

import org.abora.white.collection.steppers.Accumulator;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.xpp.basic.Heaper;

/**
 * A SetAccumulator accumulates a bunch of objects and then makes an ImmuSet containing all
 * the accumulated objects.  Several people have observed that a SetAccumulator doesn''t buy
 * you much because instead you could just store into a MuSet.  While this is true (and is in
 * fact how SetAccumulator is trivially implemented), my feeling is that if what a loop is
 * doing is enumerating a bunch of elements from which a Set is to be formed, using a
 * SetAccumulator in the loops says this more clearly to readers of the code.
 */
public class SetAccumulator extends Accumulator {
	protected MuSet muSet;
	/*
	udanax-top.st:12314:
	Accumulator subclass: #SetAccumulator
		instanceVariableNames: 'muSet {MuSet}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:12318:
	SetAccumulator comment:
	'A SetAccumulator accumulates a bunch of objects and then makes an ImmuSet containing all the accumulated objects.  Several people have observed that a SetAccumulator doesn''t buy you much because instead you could just store into a MuSet.  While this is true (and is in fact how SetAccumulator is trivially implemented), my feeling is that if what a loop is doing is enumerating a bunch of elements from which a Set is to be formed, using a SetAccumulator in the loops says this more clearly to readers of the code.'!
	*/
	/*
	udanax-top.st:12320:
	(SetAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:12362:
	SetAccumulator class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12365:
	(SetAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public void step(Heaper someObj) {
		muSet.store(someObj);
		/*
		udanax-top.st:12325:SetAccumulator methodsFor: 'accessing'!
		{void} step: someObj {Heaper}
			muSet store: someObj!
		*/
	}

	public Heaper value() {
		return muSet.asImmuSet();
		/*
		udanax-top.st:12328:SetAccumulator methodsFor: 'accessing'!
		{Heaper} value
			^ muSet asImmuSet!
		*/
	}

	public SetAccumulator() {
		super();
		muSet = MuSet.make();
		/*
		udanax-top.st:12333:SetAccumulator methodsFor: 'protected: creation'!
		create
			super create.
			muSet _ MuSet make!
		*/
	}

	public SetAccumulator(ScruSet initialSet) {
		super();
		muSet = initialSet.asMuSet();
		/*
		udanax-top.st:12337:SetAccumulator methodsFor: 'protected: creation'!
		create: initialSet {ScruSet}
			super create.
			muSet _ initialSet asMuSet!
		*/
	}

	public Accumulator copy() {
		return new SetAccumulator(muSet.asMuSet());
		/*
		udanax-top.st:12343:SetAccumulator methodsFor: 'creation'!
		{Accumulator} copy
			^ SetAccumulator create: muSet asMuSet!
		*/
	}

//	public ImmuSet get() {
//		passe();
//		/*
//		udanax-top.st:12348:SetAccumulator methodsFor: 'smalltalk: passe'!
//		{ImmuSet} get
//			self passe!
//		*/
//	}

	public SetAccumulator(Rcvr receiver) {
		super(receiver);
		muSet = (MuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:12353:SetAccumulator methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			muSet _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(muSet);
		/*
		udanax-top.st:12357:SetAccumulator methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: muSet.!
		*/
	}

	/**
	 * Make a SetAccumulator which starts out with no elements accumulated
	 */
	public static SetAccumulator make() {
		return new SetAccumulator();
		/*
		udanax-top.st:12370:SetAccumulator class methodsFor: 'instance creation'!
		{SetAccumulator} make
			"Make a SetAccumulator which starts out with no elements accumulated"
			^SetAccumulator create!
		*/
	}

	/**
	 * Make a new SetAccumulator in which all the current elements of initialSet are already
	 * accumulated.
	 * Future changes to initialSet have no effect on the accumulator.
	 */
	public static SetAccumulator make(ScruSet initialSet) {
		return new SetAccumulator(initialSet);
		/*
		udanax-top.st:12374:SetAccumulator class methodsFor: 'instance creation'!
		{SetAccumulator} make: initialSet {ScruSet}
			"Make a new SetAccumulator in which all the current elements of initialSet are already accumulated.
			Future changes to initialSet have no effect on the accumulator."
			^SetAccumulator create: initialSet!
		*/
	}
}
