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
package org.abora.white.collection.steppers;

import org.abora.white.aspire.PtrArrayAccumulator;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.xpp.basic.Heaper;

/**
 * An Accumulator is a thing which collects a sequence of objects one at a time for some
 * purpose.  Typically, this purpose is to construct a new object out of all the collected
 * objects.  When used in this way, one can think of the Accumulator as being sort of like a
 * pseudo-constructor which is spread out in time, and whose arguments are identified by the
 * sequence they occur in.  Accumulators are typically used in loops.
 * <p>
 * A (future) example of an Accumulator which is not like "a pseudo-constructor spread out in
 * time" is a communications stream between two threads (or even coroutines) managed by an
 * Accumulator / Stepper pair.  The producer process produces by putting objects into his
 * Accumulator, and the consuming process consumes by pulling values out of his Stepper.  If
 * you want to stretch the analogy, I suppose you can see the Accumulator of the pair as a
 * pseudo-constructor which constructs the Stepper, but *overlapped* in time.
 * <p>
 * It is normally considered bad style for two methods/functions to be pointing at the same
 * Acumulator.  As long as Accumulators are used locally and without aliasing (i.e., as if
 * they were pass-by-value Vars), these implementationally side-effecty objects can be
 * understood applicatively.  If a copy of an Accumulator can be passed instead of a pointer
 * to the same one, this is to be prefered.  This same comment applies even more so for
 * Steppers.
 * <p>
 * Example:  To build a set consisting of some transform of the elements of an existing set
 * (what Smalltalk would naturally do with "collect:"), a natural form for the loop would be:
 * <code>
 * 	SPTR(Accumulator) acc = setAccumulator();
 * 	FOR_EACH(Heaper,each,oldSet->stepper(), {
 * 		acc->step (transform (each));
 * 	});
 * 	return CAST(ImmuSet,acc->value());
 * </code>
 * <p>
 * See class Stepper for documentation of FOR_EACH.
 */
public abstract class Accumulator extends Heaper {
	/*
	udanax-top.st:11563:
	Heaper subclass: #Accumulator
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:11567:
	Accumulator comment:
	'An Accumulator is a thing which collects a sequence of objects one at a time for some purpose.  Typically, this purpose is to construct a new object out of all the collected objects.  When used in this way, one can think of the Accumulator as being sort of like a pseudo-constructor which is spread out in time, and whose arguments are identified by the sequence they occur in.  Accumulators are typically used in loops.
		
		A (future) example of an Accumulator which is not like "a pseudo-constructor spread out in time" is a communications stream between two threads (or even coroutines) managed by an Accumulator / Stepper pair.  The producer process produces by putting objects into his Accumulator, and the consuming process consumes by pulling values out of his Stepper.  If you want to stretch the analogy, I suppose you can see the Accumulator of the pair as a pseudo-constructor which constructs the Stepper, but *overlapped* in time.
		
		It is normally considered bad style for two methods/functions to be pointing at the same Acumulator.  As long as Accumulators are used locally and without aliasing (i.e., as if they were pass-by-value Vars), these implementationally side-effecty objects can be understood applicatively.  If a copy of an Accumulator can be passed instead of a pointer to the same one, this is to be prefered.  This same comment applies even more so for Steppers.
		
		Example:  To build a set consisting of some transform of the elements of an existing set (what Smalltalk would naturally do with "collect:"), a natural form for the loop would be:
		
		SPTR(Accumulator) acc = setAccumulator();
		FOR_EACH(Heaper,each,oldSet->stepper(), {
			acc->step (transform (each));
		});
		return CAST(ImmuSet,acc->value());
		
		See class Stepper for documentation of FOR_EACH.'!
	*/
	/*
	udanax-top.st:11583:
	(Accumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
	*/
	/*
	udanax-top.st:11613:
	Accumulator class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:11616:
	(Accumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected Accumulator() {
		super();
	}

	protected Accumulator(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	/**
	 * An accumulator that returns a PtrArray of the object put into it, in sequence
	 */
	public static Accumulator ptrArray() {
		return new PtrArrayAccumulator();
		/*
		udanax-top.st:11621:Accumulator class methodsFor: 'creation'!
		{Accumulator INLINE} ptrArray
			"An accumulator that returns a PtrArray of the object put into it, in sequence"
			
			^PtrArrayAccumulator create!
		*/
	}

	/////////////////////////////////////////////
	// Deferred Operations

	/**
	 * Accumulate a new object into the Accumulator
	 */
	public abstract void step(Heaper someObj);
	/*
	udanax-top.st:11588:Accumulator methodsFor: 'deferred operations'!
	{void} step: someObj {Heaper}
		"Accumulate a new object into the Accumulator"
		
		self subclassResponsibility!
	*/

	/**
	 * Return the object that results from accumulating all those objects
	 */
	public abstract Heaper value();
	/*
	udanax-top.st:11593:Accumulator methodsFor: 'deferred operations'!
	{Heaper} value
		"Return the object that results from accumulating all those objects"
		self subclassResponsibility!
	*/

	/**
	 * Return a new Accumulator just like the current one, except that
	 * from now on they accumulate separately
	 */
	public abstract Accumulator copy();
	/*
	udanax-top.st:11600:Accumulator methodsFor: 'deferred creation'!
	{Accumulator} copy
		"Return a new Accumulator just like the current one, except that 
		from now on they accumulate separately"
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Comparing and Hashing

	public int actualHashForEqual() {
		//TODO review
		return System.identityHashCode(this);
		//return asOop();
		/*
		udanax-top.st:11608:Accumulator methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:11610:Accumulator methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}
}
