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
package info.dgjones.abora.white.edgeregion;

import java.io.PrintWriter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Clients of EdgeManager define concrete subclasses of this, which are then used by the
 * EdgeManager code
 */
public abstract class TransitionEdge extends Heaper {
	/*
	udanax-top.st:63348:
	Heaper subclass: #TransitionEdge
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-EdgeRegion'!
	*/
	/*
	udanax-top.st:63352:
	TransitionEdge comment:
	'Clients of EdgeManager define concrete subclasses of this, which are then used by the EdgeManager code'!
	*/
	/*
	udanax-top.st:63354:
	(TransitionEdge getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected TransitionEdge() {
		super();
	}

	public TransitionEdge ceiling(TransitionEdge other) {
		if (other.isGE(this)) {
			return other;
		} else {
			return this;
		}
		/*
		udanax-top.st:63359:TransitionEdge methodsFor: 'accessing'!
		{TransitionEdge} ceiling: other {TransitionEdge}
			(other isGE: self)
				ifTrue: [^other]
				ifFalse: [^self]!
		*/
	}

	public TransitionEdge floor(TransitionEdge other) {
		if (isGE(other)) {
			return other;
		} else {
			return this;
		}
		/*
		udanax-top.st:63365:TransitionEdge methodsFor: 'accessing'!
		{TransitionEdge} floor: other {TransitionEdge}
			(self isGE: other)
				ifTrue: [^other]
				ifFalse: [^self]!
		*/
	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
//		return Heaper.takeOop();
		/*
		udanax-top.st:63373:TransitionEdge methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	/**
	 * Whether the position is strictly less than this edge
	 */
	public abstract boolean follows(Position pos);
		/*
		udanax-top.st:63377:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} follows: pos {Position}
			"Whether the position is strictly less than this edge"
			
			self subclassResponsibility!
		*/
	

	public abstract boolean isEqual(Heaper other);
		/*
		udanax-top.st:63382:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			self subclassResponsibility!
		*/
	

	/**
	 * Whether there is precisely one position between this edge and the next one
	 */
	public abstract boolean isFollowedBy(TransitionEdge next);
		/*
		udanax-top.st:63386:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} isFollowedBy: next {TransitionEdge}
			"Whether there is precisely one position between this edge and the next one"
			
			self subclassResponsibility!
		*/
	

	/**
	 * Defines a full ordering among all edges in a given CoordinateSpace
	 */
	public abstract boolean isGE(TransitionEdge other);
		/*
		udanax-top.st:63391:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} isGE: other {TransitionEdge}
			"Defines a full ordering among all edges in a given CoordinateSpace"
			
			self subclassResponsibility!
		*/
	

	/**
	 * Whether this edge touches the same position the other does
	 */
	public abstract boolean touches(TransitionEdge other);
		/*
		udanax-top.st:63396:TransitionEdge methodsFor: 'testing'!
		{BooleanVar} touches: other {TransitionEdge}
			"Whether this edge touches the same position the other does"
			
			self subclassResponsibility!
		*/
	

	/**
	 * Print a description of this transition
	 */
	public abstract void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious);
		/*
		udanax-top.st:63403:TransitionEdge methodsFor: 'printing'!
		{void} printTransitionOn: oo {ostream reference}
			with: entering {BooleanVar}
			with: touchesPrevious {BooleanVar}
			"Print a description of this transition"
			
			self subclassResponsibility!
		*/
	

	public TransitionEdge(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:63412:TransitionEdge methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:63415:TransitionEdge methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
