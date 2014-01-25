/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.white.tumbler;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.edge.EdgeManager;
import info.dgjones.abora.white.edgeregion.TransitionEdge;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.PrimSpec;

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
