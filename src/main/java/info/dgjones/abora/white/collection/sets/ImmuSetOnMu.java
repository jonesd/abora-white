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
package info.dgjones.abora.white.collection.sets;

import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Implementation of ImmuSet by delegating to an internal MuSet which actually
 * holds the members of the set.
 */
public class ImmuSetOnMu extends ImmuSet {
	protected MuSet setInternal;
	/*
	udanax-top.st:45608:
	ImmuSet subclass: #ImmuSetOnMu
		instanceVariableNames: 'setInternal {MuSet}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:45612:
	(ImmuSetOnMu getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:45710:
	ImmuSetOnMu class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:45713:
	(ImmuSetOnMu getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	/**
	 * this set should be a copy for my own use
	 */
	protected ImmuSetOnMu(MuSet fromSet) {
		/* the pseudo constructor enforces this */
		super();
		setInternal = fromSet;
		/*
		udanax-top.st:45689:ImmuSetOnMu methodsFor: 'protected: create'!
		create.MuSet: fromSet {MuSet}
			"this set should be a copy for my own use"
			"the pseudo constructor enforces this"
			super create.
			setInternal _ fromSet!
		*/
	}

	protected ImmuSetOnMu(Rcvr receiver) {
		super(receiver);
		setInternal = (MuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:45701:ImmuSetOnMu methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			setInternal _ receiver receiveHeaper.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static ImmuSet make(MuSet aSet) {
		//TODO aSet should really be a copy for normal users
		return new ImmuSetOnMu(aSet);
		/*
		udanax-top.st:45718:ImmuSetOnMu class methodsFor: 'creation'!
		{ImmuSet} make: aSet {MuSet}
			^ self create.MuSet: aSet!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public boolean hasMember(Heaper someone) {
		return setInternal.hasMember(someone);
		/*
		udanax-top.st:45617:ImmuSetOnMu methodsFor: 'accessing'!
		{BooleanVar} hasMember: someone {Heaper}
			^ setInternal hasMember: someone!
		*/
	}

	public boolean isEmpty() {
		return setInternal.isEmpty();
		/*
		udanax-top.st:45620:ImmuSetOnMu methodsFor: 'accessing'!
		{BooleanVar} isEmpty
			^ setInternal isEmpty!
		*/
	}

	public boolean isSubsetOf(ScruSet another) {
		return setInternal.isSubsetOf(another);
		/*
		udanax-top.st:45623:ImmuSetOnMu methodsFor: 'accessing'!
		{BooleanVar} isSubsetOf: another {ScruSet}
			^ setInternal isSubsetOf: another!
		*/
	}

	/////////////////////////////////////////////
	// Enumerating

	public IntegerValue count() {
		return setInternal.count();
		/*
		udanax-top.st:45628:ImmuSetOnMu methodsFor: 'enumerating'!
		{IntegerVar} count
			^ setInternal count!
		*/
	}

	public Stepper stepper() {
		return setInternal.stepper();
		/*
		udanax-top.st:45631:ImmuSetOnMu methodsFor: 'enumerating'!
		{Stepper} stepper
			^ setInternal stepper!
		*/
	}

	public Heaper theOne() {
		return setInternal.theOne();
		/*
		udanax-top.st:45634:ImmuSetOnMu methodsFor: 'enumerating'!
		{Heaper} theOne
			^ setInternal theOne!
		*/
	}

	/////////////////////////////////////////////
	// Operations

	public ImmuSet intersect(ScruSet another) {
		if (another.isEmpty()) {
			return ImmuSet.make();
		} else {
			MuSet tmp;
			tmp = (MuSet) setInternal.copy();
			tmp.restrictTo(another);
			return ImmuSet.from(tmp);
		}
		/*
		udanax-top.st:45639:ImmuSetOnMu methodsFor: 'operations'!
		{ImmuSet} intersect: another {ScruSet} 
			another isEmpty
				ifTrue: [ ^ ImmuSet make ]
				ifFalse:
					[| tmp {MuSet} |
					tmp _ (setInternal copy) quickCast: MuSet.
					tmp restrictTo: another.
					^ ImmuSet from: tmp]!
		*/
	}

	public ImmuSet minus(ScruSet another) {
		if (another.isEmpty()) {
			return this;
		} else {
			MuSet tmp = (MuSet) setInternal.copy();
			tmp.wipeAll(another);
			return ImmuSet.from(tmp);
		}
		/*
		udanax-top.st:45648:ImmuSetOnMu methodsFor: 'operations'!
		{ImmuSet} minus: another {ScruSet}
			another isEmpty
				ifTrue: [ ^ self ]
				ifFalse:
					[|tmp {MuSet} |
					tmp _ (setInternal copy) quickCast: MuSet.
					tmp wipeAll: another.
					^ ImmuSet from: tmp]!
		*/
	}

	public ImmuSet unionWith(ScruSet another) {
		if (another.isEmpty()) {
			return this;
		} else {
			if (setInternal.count().isLT(another.count())) {
				return another.asImmuSet().unionWith(setInternal);
			}
			MuSet tmp = (MuSet) setInternal.copy();
			tmp.storeAll(another);
			return ImmuSet.from(tmp);
		}
		/*
		udanax-top.st:45657:ImmuSetOnMu methodsFor: 'operations'!
		{ImmuSet} unionWith: another {ScruSet}
			another isEmpty
				ifTrue: [ ^ self ]
				ifFalse:
					[| tmp {MuSet} |
					setInternal count < another count ifTrue: [^another asImmuSet unionWith: setInternal].
					tmp _ setInternal copy quickCast: MuSet.
					tmp storeAll: another.
					^ ImmuSet from: tmp]!
		*/
	}

	/////////////////////////////////////////////
	// Adding-Removing

	public ImmuSet with(Heaper anElement) {
		MuSet tmp = asMuSet();
		tmp.store(anElement);
		return new ImmuSetOnMu(tmp);
		/*
		udanax-top.st:45669:ImmuSetOnMu methodsFor: 'adding-removing'!
		{ImmuSet} with: anElement {Heaper}
			|tmp {MuSet} |
			tmp _ self asMuSet.
			tmp store: anElement.
			^ ImmuSetOnMu create.MuSet: tmp!
		*/
	}

	public ImmuSet without(Heaper anElement) {
		MuSet tmp = (MuSet) setInternal.copy();
		tmp.wipe(anElement);
		return ImmuSet.from(tmp);
		/*
		udanax-top.st:45675:ImmuSetOnMu methodsFor: 'adding-removing'!
		{ImmuSet} without: anElement {Heaper}
			
			|tmp {MuSet} |
			tmp _ (setInternal copy) quickCast: MuSet.
			tmp wipe: anElement.
			^ ImmuSet from: tmp!
		*/
	}

	/////////////////////////////////////////////
	// Conversion

	public MuSet asMuSet() {
		return (MuSet) setInternal.copy();
		/*
		udanax-top.st:45684:ImmuSetOnMu methodsFor: 'conversion'!
		{MuSet} asMuSet
			^ (setInternal copy) cast: MuSet!
		*/
	}

	/////////////////////////////////////////////
	// Create

	public void destruct() {
		setInternal.destroy();
		super.destruct();
		/*
		udanax-top.st:45695:ImmuSetOnMu methodsFor: 'protected: create'!
		{void} destruct
			setInternal destroy.
			super destruct!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(setInternal);
		/*
		udanax-top.st:45705:ImmuSetOnMu methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: setInternal.!
		*/
	}
}
