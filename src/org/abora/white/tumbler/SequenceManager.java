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

import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.edge.EdgeManager;
import org.abora.white.edgeregion.TransitionEdge;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.PrimSpec;

/**
 * Specialized object for managing TumblerSpace objects. Is a type so that inlining could
 * potentially be used.
 */
public class SequenceManager extends EdgeManager {
	/*
	udanax-top.st:18588:
	EdgeManager subclass: #SequenceManager
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:18592:
	SequenceManager comment:
	'Specialized object for managing TumblerSpace objects. Is a type so that inlining could potentially be used.'!
	*/
	/*
	udanax-top.st:18594:
	(SequenceManager getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public Position edgePosition(TransitionEdge edge) {
		return ((SequenceEdge) edge).sequence();
		/*
		udanax-top.st:18599:SequenceManager methodsFor: 'protected:'!
		{Position} edgePosition: edge {TransitionEdge}
			^(edge cast: SequenceEdge) sequence!
		*/
	}

	public XnRegion makeNew(boolean startsInside, PtrArray transitions) {
		return makeNew(startsInside, transitions, transitions.count());
		/*
		udanax-top.st:18603:SequenceManager methodsFor: 'protected:'!
		{XnRegion} makeNew: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge}
			^self makeNew: startsInside with: transitions with: transitions count!
		*/
	}

	public XnRegion makeNew(boolean startsInside, PtrArray transitions, int count) {
		return new SequenceRegion(startsInside, transitions, count);
		/*
		udanax-top.st:18607:SequenceManager methodsFor: 'protected:'!
		{XnRegion} makeNew: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge} with: count {Int32}
			^SequenceRegion create: startsInside with: transitions with: count!
		*/
	}

	public PtrArray posTransitions(Position pos) {
		return (PtrArray) (PrimSpec.pointer().arrayWithTwo((BeforeSequence.make(((Sequence) pos))), (AfterSequence.make(((Sequence) pos)))));
		/*
		udanax-top.st:18611:SequenceManager methodsFor: 'protected:'!
		{PtrArray of: TransitionEdge} posTransitions: pos {Position}
			^ (PrimSpec pointer
				arrayWithTwo: (BeforeSequence make: (pos cast: Sequence))
				with: (AfterSequence make: (pos cast: Sequence))) cast: PtrArray!
		*/
	}

	public boolean startsInside(XnRegion region) {
		return ((SequenceRegion) region).startsInside();
		/*
		udanax-top.st:18617:SequenceManager methodsFor: 'protected:'!
		{BooleanVar} startsInside: region {XnRegion}
			^(region cast: SequenceRegion) startsInside!
		*/
	}

	public PtrArray transitions(XnRegion region) {
		return ((SequenceRegion) region).secretTransitions();
		/*
		udanax-top.st:18621:SequenceManager methodsFor: 'protected:'!
		{PtrArray of: TransitionEdge} transitions: region {XnRegion}
			^(region cast: SequenceRegion) secretTransitions!
		*/
	}

	public int transitionsCount(XnRegion region) {
		return ((SequenceRegion) region).secretTransitionsCount();
		/*
		udanax-top.st:18625:SequenceManager methodsFor: 'protected:'!
		{Int32} transitionsCount: region {XnRegion}
			^(region cast: SequenceRegion) secretTransitionsCount!
		*/
	}

	public SequenceManager() {
		super();
	}

	public SequenceManager(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:18631:SequenceManager methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:18634:SequenceManager methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
