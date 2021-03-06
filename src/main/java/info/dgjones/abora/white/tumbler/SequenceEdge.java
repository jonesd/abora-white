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

import java.io.PrintWriter;

import info.dgjones.abora.white.edgeregion.TransitionEdge;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.xpp.basic.Heaper;

public abstract class SequenceEdge extends TransitionEdge {
	protected Sequence mySequence;
	/*
	udanax-top.st:63627:
	TransitionEdge subclass: #SequenceEdge
		instanceVariableNames: 'mySequence {Sequence}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:63631:
	(SequenceEdge getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
	*/

	public int actualHashForEqual() {
		return sequence().hashForEqual() ^ getClass().hashCode();
		/*
		udanax-top.st:63636:SequenceEdge methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^self sequence hashForEqual bitXor: self getCategory hashForEqual!
		*/
	}

	/**
	 * Whether the position is strictly less than this edge
	 */
	public abstract boolean follows(Position pos);
	/*
	udanax-top.st:63640:SequenceEdge methodsFor: 'testing'!
	{BooleanVar} follows: pos {Position}
		"Whether the position is strictly less than this edge"
		
		self subclassResponsibility!
	*/

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:63645:SequenceEdge methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		self subclassResponsibility!
	*/

	/**
	 * Whether there is precisely one position between this edge and the next one
	 */
	public abstract boolean isFollowedBy(TransitionEdge next);
	/*
	udanax-top.st:63649:SequenceEdge methodsFor: 'testing'!
	{BooleanVar} isFollowedBy: next {TransitionEdge}
		"Whether there is precisely one position between this edge and the next one"
		
		self subclassResponsibility!
	*/

	/**
	 * Defines a full ordering among all edges in a given CoordinateSpace
	 */
	public abstract boolean isGE(TransitionEdge other);
	/*
	udanax-top.st:63654:SequenceEdge methodsFor: 'testing'!
	{BooleanVar} isGE: other {TransitionEdge}
		"Defines a full ordering among all edges in a given CoordinateSpace"
		
		self subclassResponsibility!
	*/

	/**
	 * Whether this edge touches the same position the other does
	 */
	public abstract boolean touches(TransitionEdge other);
	/*
	udanax-top.st:63659:SequenceEdge methodsFor: 'testing'!
	{BooleanVar} touches: other {TransitionEdge}
		"Whether this edge touches the same position the other does"
		
		self subclassResponsibility!
	*/

	public Sequence sequence() {
		return mySequence;
		/*
		udanax-top.st:63666:SequenceEdge methodsFor: 'accessing'!
		{Sequence} sequence
			^mySequence!
		*/
	}

	/**
	 * Transform the edge by the given mapping
	 */
	public abstract SequenceEdge transformedBy(SequenceMapping dsp);
	/*
	udanax-top.st:63670:SequenceEdge methodsFor: 'accessing'!
	{SequenceEdge} transformedBy: dsp {SequenceMapping}
		"Transform the edge by the given mapping"
		
		self subclassResponsibility!
	*/

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(mySequence);
		oo.print(")");
		/*
		udanax-top.st:63677:SequenceEdge methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << mySequence << ')'!
		*/
	}

	/**
	 * Print a description of this transition
	 */
	public abstract void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious);
	/*
	udanax-top.st:63681:SequenceEdge methodsFor: 'printing'!
	{void} printTransitionOn: oo {ostream reference}
		with: entering {BooleanVar}
		with: touchesPrevious {BooleanVar}
		"Print a description of this transition"
		
		self subclassResponsibility!
	*/

	public SequenceEdge(Sequence sequence) {
		super();
		mySequence = sequence;
		/*
		udanax-top.st:63690:SequenceEdge methodsFor: 'create'!
		create: sequence {Sequence}
			super create.
			mySequence := sequence.!
		*/
	}

	public SequenceEdge(Rcvr receiver) {
		super(receiver);
		mySequence = (Sequence) receiver.receiveHeaper();
		/*
		udanax-top.st:63697:SequenceEdge methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			mySequence _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(mySequence);
		/*
		udanax-top.st:63701:SequenceEdge methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: mySequence.!
		*/
	}
}
