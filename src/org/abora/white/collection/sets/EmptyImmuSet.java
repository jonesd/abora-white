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

import org.abora.white.collection.steppers.Stepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class EmptyImmuSet extends ImmuSet {
	/*
	udanax-top.st:45519:
	ImmuSet subclass: #EmptyImmuSet
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:45523:
	(EmptyImmuSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:45596:
	EmptyImmuSet class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:45599:
	(EmptyImmuSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected EmptyImmuSet() {
		super();
		/*
		udanax-top.st:45575:EmptyImmuSet methodsFor: 'unprotected for initer create'!
		create
			super create!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	public static Heaper make(Rcvr rcvr) {
		throw new UnsupportedOperationException();
		//		((SpecialistRcvr) rcvr).registerIbid(ImmuSet.make());
		//		return ImmuSet.make();
		/*
		udanax-top.st:45604:EmptyImmuSet class methodsFor: 'rcvr pseudo constructor'!
		{Heaper} make.Rcvr: rcvr {Rcvr}
			(rcvr cast: SpecialistRcvr) registerIbid: ImmuSet make.
			^ImmuSet make!
		*/
	}

	/////////////////////////////////////////////
	// Enumerating

	public IntegerValue count() {
		return IntegerValue.zero();
		/*
		udanax-top.st:45528:EmptyImmuSet methodsFor: 'enumerating'!
		{IntegerVar} count
			^ IntegerVar0!
		*/
	}

	public Stepper stepper() {
		return Stepper.emptyStepper();
		/*
		udanax-top.st:45531:EmptyImmuSet methodsFor: 'enumerating'!
		{Stepper} stepper
			^ Stepper emptyStepper!
		*/
	}

	public Heaper theOne() {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
		/*
		udanax-top.st:45534:EmptyImmuSet methodsFor: 'enumerating'!
		{Heaper} theOne
			Heaper BLAST: #NotOneElement.
			^ NULL!
		*/
	}

	/////////////////////////////////////////////
	// Adding-Removing

	public ImmuSet with(Heaper anElement) {
		return TinyImmuSet.make(anElement);
		/*
		udanax-top.st:45540:EmptyImmuSet methodsFor: 'adding-removing'!
		{ImmuSet} with: anElement {Heaper}
			^TinyImmuSet make: anElement!
		*/
	}

	public ImmuSet without(Heaper anElement) {
		return this;
		/*
		udanax-top.st:45543:EmptyImmuSet methodsFor: 'adding-removing'!
		{ImmuSet} without: anElement {Heaper unused}
			^ self!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public boolean hasMember(Heaper someone) {
		return false;
		/*
		udanax-top.st:45548:EmptyImmuSet methodsFor: 'accessing'!
		{BooleanVar} hasMember: someone {Heaper unused}
			^ false!
		*/
	}

	public boolean isEmpty() {
		return true;
		/*
		udanax-top.st:45551:EmptyImmuSet methodsFor: 'accessing'!
		{BooleanVar} isEmpty
			^ true!
		*/
	}

	public boolean isSubsetOf(ScruSet another) {
		return true;
		/*
		udanax-top.st:45554:EmptyImmuSet methodsFor: 'accessing'!
		{BooleanVar} isSubsetOf: another {ScruSet unused}
			^ true!
		*/
	}

	/////////////////////////////////////////////
	// Operations

	public ImmuSet intersect(ScruSet another) {
		return this;
		/*
		udanax-top.st:45559:EmptyImmuSet methodsFor: 'operations'!
		{ImmuSet} intersect: another {ScruSet unused} 
			^ self!
		*/
	}

	public ImmuSet minus(ScruSet another) {
		return this;
		/*
		udanax-top.st:45562:EmptyImmuSet methodsFor: 'operations'!
		{ImmuSet} minus: another {ScruSet unused}
			^ self!
		*/
	}

	public ImmuSet unionWith(ScruSet another) {
		return another.asImmuSet();
		/*
		udanax-top.st:45565:EmptyImmuSet methodsFor: 'operations'!
		{ImmuSet} unionWith: another {ScruSet}
			^ another asImmuSet!
		*/
	}

	public MuSet asMuSet() {
		return MuSet.make();
		/*
		udanax-top.st:45570:EmptyImmuSet methodsFor: 'conversion'!
		{MuSet} asMuSet
			^ MuSet make!
		*/
	}

	/**
	 * Don't destroy our single instance
	 */
	public void destroy() {
		/*
		udanax-top.st:45580:EmptyImmuSet methodsFor: 'creation'!
		{void} destroy
			"Don't destroy our single instance"!
		*/
	}

	/**
	 * This object is a canonical single instance, so its destructor should only be called after
	 * main has exited.
	 */
	public void destruct() {
		/* translateOnly "if (!!Initializer::inStaticDestruction()) BLAST(SanityViolation);" */
		super.destruct();
		/*
		udanax-top.st:45585:EmptyImmuSet methodsFor: 'protected: destruct'!
		{void} destruct
			"This object is a canonical single instance, so its destructor should only be called after main has exited."
			 
			'if (!!Initializer::inStaticDestruction()) BLAST(SanityViolation);' translateOnly.
			super destruct!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		/*
		udanax-top.st:45593:EmptyImmuSet methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}!
		*/
	}
}
