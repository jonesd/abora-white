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
 * Like a SetAccumulator, a UnionRecruiter makes an ImmuSet out of the things that it
 * Accumulates.  However, the things that a UnionRecruiter accumulates must themselves be
 * ScruSets, and the resulting ImmuSet consists of the union of the elements of each of the
 * accumulated sets as of the time they were accumulated.
 */
public class UnionRecruiter extends Accumulator {
	protected MuSet muSet;
	/*
	udanax-top.st:12474:
	Accumulator subclass: #UnionRecruiter
		instanceVariableNames: 'muSet {MuSet}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:12478:
	UnionRecruiter comment:
	'Like a SetAccumulator, a UnionRecruiter makes an ImmuSet out of the things that it Accumulates.  However, the things that a UnionRecruiter accumulates must themselves be ScruSets, and the resulting ImmuSet consists of the union of the elements of each of the accumulated sets as of the time they were accumulated.'!
	*/
	/*
	udanax-top.st:12480:
	(UnionRecruiter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:12516:
	UnionRecruiter class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12519:
	(UnionRecruiter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public void step(Heaper someObj) {
		muSet.storeAll(((ScruSet) someObj));
		/*
		udanax-top.st:12485:UnionRecruiter methodsFor: 'accessing'!
		{void} step: someObj {Heaper}
			muSet storeAll: (someObj cast: ScruSet)!
		*/
	}

	public Heaper value() {
		return muSet.asImmuSet();
		/*
		udanax-top.st:12488:UnionRecruiter methodsFor: 'accessing'!
		{Heaper} value
			^ muSet asImmuSet!
		*/
	}

	protected UnionRecruiter() {
		super();
		muSet = MuSet.make();
		/*
		udanax-top.st:12493:UnionRecruiter methodsFor: 'protected: creation'!
		create
			super create.
			muSet _ MuSet make!
		*/
	}

	public Accumulator copy() {
		Accumulator result = UnionRecruiter.make();
		result.step(muSet);
		return result;
		/*
		udanax-top.st:12499:UnionRecruiter methodsFor: 'creation'!
		{Accumulator} copy
			| result {Accumulator} |
			result _ UnionRecruiter make.
			result step: muSet.
			^result!
		*/
	}

	protected UnionRecruiter(Rcvr receiver) {
		super(receiver);
		muSet = (MuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:12507:UnionRecruiter methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			muSet _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(muSet);
		/*
		udanax-top.st:12511:UnionRecruiter methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: muSet.!
		*/
	}

	/**
	 * Make a new UnionRecruiter which hasn't yet accumulated anything
	 */
	public static UnionRecruiter make() {
		return new UnionRecruiter();
		/*
		udanax-top.st:12524:UnionRecruiter class methodsFor: 'pseudo constructors'!
		{UnionRecruiter} make
			"Make a new UnionRecruiter which hasn't yet accumulated anything"
			^UnionRecruiter create!
		*/
	}
}
