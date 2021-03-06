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

public class AfterReal extends RealEdge {
	/*
	udanax-top.st:63482:
	RealEdge subclass: #AfterReal
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:63486:
	(AfterReal getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:63538:
	AfterReal class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:63541:
	(AfterReal getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	public boolean follows(Position pos) {
		return position().isGE(((RealPos) pos));
		/*
		udanax-top.st:63491:AfterReal methodsFor: 'comparing'!
		{BooleanVar} follows: pos {Position}
			
			^self position isGE: (pos cast: RealPos)!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof AfterReal) {
			AfterReal after = (AfterReal) other;
			return position().isEqual(after.position());
		} else {
			return false;
		}
		/*
		udanax-top.st:63495:AfterReal methodsFor: 'comparing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: AfterReal into: [:after |
					^self position isEqual: after position]
				others: [^false].
			^false "fodder"!
		*/
	}

	public boolean isFollowedBy(TransitionEdge next) {
		return false;
		/*
		udanax-top.st:63503:AfterReal methodsFor: 'comparing'!
		{BooleanVar} isFollowedBy: next {TransitionEdge}
			^false!
		*/
	}

	public boolean isGE(TransitionEdge other) {
		return position().isGE(((RealEdge) other).position());
		/*
		udanax-top.st:63507:AfterReal methodsFor: 'comparing'!
		{BooleanVar} isGE: other {TransitionEdge}
			
			^self position isGE: (other cast: RealEdge) position!
		*/
	}

	public void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious) {
		oo.print(" ");
		if (entering) {
			oo.print("(");
		}
		if (!(touchesPrevious && (!entering))) {
			oo.print(position());
		}
		if (!entering) {
			oo.print("]");
		}
		/*
		udanax-top.st:63513:AfterReal methodsFor: 'printing'!
		{void} printTransitionOn: oo {ostream reference}
			with: entering {BooleanVar}
			with: touchesPrevious {BooleanVar}
			
			oo << ' '.
			entering ifTrue: [oo << '('].
			(touchesPrevious and: [entering not]) ifFalse:
				[oo << self position].
			entering ifFalse: [oo << ']']!
		*/
	}

	public AfterReal(RealPos pos) {
		super(pos);
		/*
		udanax-top.st:63525:AfterReal methodsFor: 'creation'!
		create: pos {RealPos}
			super create: pos.!
		*/
	}

	public AfterReal(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:63531:AfterReal methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:63534:AfterReal methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static RealEdge make(RealPos pos) {
		return new AfterReal(pos);
		/*
		udanax-top.st:63546:AfterReal class methodsFor: 'create'!
		{RealEdge} make: pos {RealPos}
			^self create: pos!
		*/
	}
}
