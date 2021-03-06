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

public class RealManager extends EdgeManager {
	/*
	udanax-top.st:18542:
	EdgeManager subclass: #RealManager
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:18546:
	(RealManager getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public Position edgePosition(TransitionEdge edge) {
		return ((RealEdge) edge).position();
		/*
		udanax-top.st:18551:RealManager methodsFor: 'protected:'!
		{Position} edgePosition: edge {TransitionEdge}
			
			^(edge cast: RealEdge) position!
		*/
	}

	public XnRegion makeNew(boolean startsInside, PtrArray transitions) {
		return RealRegion.make(startsInside, transitions);
		/*
		udanax-top.st:18555:RealManager methodsFor: 'protected:'!
		{XnRegion} makeNew: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge}
			^RealRegion make: startsInside with: transitions!
		*/
	}

	public XnRegion makeNew(boolean startsInside, PtrArray transitions, int count) {
		return makeNew(startsInside, ((PtrArray) (transitions.copy(count))));
		/*
		udanax-top.st:18559:RealManager methodsFor: 'protected:'!
		{XnRegion} makeNew: startsInside {BooleanVar} with: transitions {PtrArray of: TransitionEdge} with: count {Int32}
			^self makeNew: startsInside with: ((transitions copy: count) cast: PtrArray)!
		*/
	}

	public PtrArray posTransitions(Position pos) {
		throw new UnsupportedOperationException();
//		unimplemented();
//		return null
//		/* fodder */;
		/*
		udanax-top.st:18563:RealManager methodsFor: 'protected:'!
		{PtrArray of: TransitionEdge} posTransitions: pos {Position}
			self unimplemented.
			^NULL "fodder"!
		*/
	}

	public boolean startsInside(XnRegion region) {
		return ((RealRegion) region).startsInside();
		/*
		udanax-top.st:18568:RealManager methodsFor: 'protected:'!
		{BooleanVar} startsInside: region {XnRegion}
			^(region cast: RealRegion) startsInside!
		*/
	}

	public PtrArray transitions(XnRegion region) {
		return ((RealRegion) region).secretTransitions();
		/*
		udanax-top.st:18572:RealManager methodsFor: 'protected:'!
		{PtrArray of: TransitionEdge} transitions: region {XnRegion}
			^(region cast: RealRegion) secretTransitions!
		*/
	}

	public int transitionsCount(XnRegion region) {
		return ((RealRegion) region).secretTransitions().count();
		/*
		udanax-top.st:18576:RealManager methodsFor: 'protected:'!
		{Int32 INLINE} transitionsCount: region {XnRegion}
			^(region cast: RealRegion) secretTransitions count!
		*/
	}

	protected RealManager() {
		super();
	}

	protected RealManager(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:18582:RealManager methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:18585:RealManager methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
