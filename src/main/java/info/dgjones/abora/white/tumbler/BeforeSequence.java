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

public class BeforeSequence extends SequenceEdge {
	/*
	udanax-top.st:63796:
	SequenceEdge subclass: #BeforeSequence
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:63800:
	(BeforeSequence getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:63880:
	BeforeSequence class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:63883:
	(BeforeSequence getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	public boolean follows(Position pos) {
		return !(((Sequence) pos).isGE(sequence()));
		/*
		udanax-top.st:63805:BeforeSequence methodsFor: 'comparing'!
		{BooleanVar} follows: pos {Position}
			^((pos cast: Sequence) isGE: self sequence) not!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof BeforeSequence) {
			BeforeSequence before = (BeforeSequence) other;
			return before.sequence().isEqual(sequence());
		} else {
			return false;
		}
		/*
		udanax-top.st:63809:BeforeSequence methodsFor: 'comparing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: BeforeSequence into: [ :before |
				^before sequence isEqual: self sequence]
			others:
				[^false].
			^ false "compiler fodder"!
		*/
	}

	public boolean isFollowedBy(TransitionEdge next) {
		if (next instanceof AfterSequence) {
			AfterSequence after = (AfterSequence) next;
			return sequence().isEqual(after.sequence());
		} else {
			return false;
		}
		/*
		udanax-top.st:63817:BeforeSequence methodsFor: 'comparing'!
		{BooleanVar} isFollowedBy: next {TransitionEdge}
			next cast: AfterSequence into: [ :after |
				^self sequence isEqual: after sequence]
			others:
				[^false].
			^ false "compiler fodder"!
		*/
	}

	public boolean isGE(TransitionEdge other) {
		if (other instanceof BeforeSequencePrefix) {
			BeforeSequencePrefix prefix = (BeforeSequencePrefix) other;
			return (sequence().comparePrefix(prefix.sequence(), prefix.limit())) >= 0;
		} else if (other instanceof BeforeSequence) {
			BeforeSequence before = (BeforeSequence) other;
			return sequence().isGE(before.sequence());
		} else if (other instanceof AfterSequence) {
			AfterSequence after = (AfterSequence) other;
			return !(after.sequence().isGE(sequence()));
		} else {
			throw new IllegalArgumentException();
		}
		/*
		udanax-top.st:63825:BeforeSequence methodsFor: 'comparing'!
		{BooleanVar} isGE: other {TransitionEdge}
			other cast: BeforeSequencePrefix into: [ :prefix |
				^(self sequence comparePrefix: prefix sequence with: prefix limit) >= Int32Zero]
			cast: BeforeSequence into: [ :before |
				^self sequence isGE: before sequence]
			cast: AfterSequence into: [ :after |
				^(after sequence isGE: self sequence) not].
			^ false "compiler fodder"!
		*/
	}

	public boolean touches(TransitionEdge other) {
		if (other instanceof BeforeSequencePrefix) {
			BeforeSequencePrefix prefix = (BeforeSequencePrefix) other;
			return false;
		} else if (other instanceof SequenceEdge) {
			SequenceEdge edge = (SequenceEdge) other;
			return sequence().isEqual(edge.sequence());
		} else {
			throw new IllegalArgumentException();
		}
		/*
		udanax-top.st:63835:BeforeSequence methodsFor: 'comparing'!
		{BooleanVar} touches: other {TransitionEdge}
			other cast: BeforeSequencePrefix into: [ :prefix |
				^false]
			cast: SequenceEdge into: [ :edge |
				^self sequence isEqual: edge sequence].
			^ false "compiler fodder"!
		*/
	}

	public Position position() {
		return sequence();
		/*
		udanax-top.st:63845:BeforeSequence methodsFor: 'accessing'!
		{Position} position
			^self sequence!
		*/
	}

	public SequenceEdge transformedBy(SequenceMapping dsp) {
		return BeforeSequence.make(((Sequence) (dsp.of(sequence()))));
		/*
		udanax-top.st:63849:BeforeSequence methodsFor: 'accessing'!
		{SequenceEdge} transformedBy: dsp {SequenceMapping}
			^BeforeSequence make: ((dsp of: self sequence) cast: Sequence)!
		*/
	}

	public BeforeSequence(Sequence sequence) {
		super(sequence);
		/*
		udanax-top.st:63855:BeforeSequence methodsFor: 'create'!
		create: sequence {Sequence}
			super create: sequence.!
		*/
	}

	public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
		oo.print(" ");
		if (entering) {
			oo.print("[");
		}
		if (!(touchesPrevious && (!entering))) {
			oo.print(sequence());
		}
		if (!entering) {
			oo.print(")");
		}
		/*
		udanax-top.st:63861:BeforeSequence methodsFor: 'printing'!
		{void} printTransitionOn: oo {ostream reference}
			with: entering {BooleanVar}
			with: touchesPrevious {BooleanVar}
			oo << ' '.
			entering ifTrue: [oo << '['].
			(touchesPrevious and: [entering not]) ifFalse:
				[oo << self sequence].
			entering ifFalse: [oo << ')']!
		*/
	}

	public BeforeSequence(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:63873:BeforeSequence methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:63876:BeforeSequence methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static SequenceEdge make(Sequence sequence) {
		return new BeforeSequence(sequence);
		/*
		udanax-top.st:63888:BeforeSequence class methodsFor: 'pseudo constructors'!
		{SequenceEdge} make: sequence {Sequence}
			^self create: sequence!
		*/
	}
}
