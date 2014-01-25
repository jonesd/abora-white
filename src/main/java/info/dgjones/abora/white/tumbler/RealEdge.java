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

public abstract class RealEdge extends TransitionEdge {
	protected RealPos myPos;
	/*
	udanax-top.st:63418:
	TransitionEdge subclass: #RealEdge
		instanceVariableNames: 'myPos {RealPos}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:63422:
	(RealEdge getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
	*/

	public RealPos position() {
		return myPos;
		/*
		udanax-top.st:63427:RealEdge methodsFor: 'accessing'!
		{RealPos} position
			^myPos!
		*/
	}

	public int actualHashForEqual() {
		return myPos.hashForEqual() ^ getClass().hashCode();
		/*
		udanax-top.st:63433:RealEdge methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^myPos hashForEqual bitXor: self getCategory hashForEqual!
		*/
	}

	public abstract boolean follows(Position pos);
	/*
	udanax-top.st:63437:RealEdge methodsFor: 'testing'!
	{BooleanVar} follows: pos {Position}
		
		self subclassResponsibility!
	*/

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:63441:RealEdge methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		self subclassResponsibility!
	*/

	public abstract boolean isFollowedBy(TransitionEdge next);
	/*
	udanax-top.st:63445:RealEdge methodsFor: 'testing'!
	{BooleanVar} isFollowedBy: next {TransitionEdge}
		
		self subclassResponsibility!
	*/

	public abstract boolean isGE(TransitionEdge other);
	/*
	udanax-top.st:63449:RealEdge methodsFor: 'testing'!
	{BooleanVar} isGE: other {TransitionEdge}
		
		self subclassResponsibility!
	*/

	public boolean touches(TransitionEdge other) {
		return myPos.isEqual(((RealEdge) other).position());
		/*
		udanax-top.st:63453:RealEdge methodsFor: 'testing'!
		{BooleanVar} touches: other {TransitionEdge}
			
			^myPos isEqual: (other cast: RealEdge) position!
		*/
	}

	public abstract void printTransitionOn(PrintWriter oo, boolean entering, boolean touchesPrevious);
	/*
	udanax-top.st:63459:RealEdge methodsFor: 'printing'!
	{void} printTransitionOn: oo {ostream reference}
		with: entering {BooleanVar}
		with: touchesPrevious {BooleanVar}
		
		self subclassResponsibility!
	*/

	public RealEdge(RealPos pos) {
		super();
		myPos = pos;
		/*
		udanax-top.st:63467:RealEdge methodsFor: 'creation'!
		create: pos {RealPos}
			super create.
			myPos := pos.!
		*/
	}

	public RealEdge(Rcvr receiver) {
		super(receiver);
		myPos = (RealPos) receiver.receiveHeaper();
		/*
		udanax-top.st:63474:RealEdge methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myPos _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myPos);
		/*
		udanax-top.st:63478:RealEdge methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myPos.!
		*/
	}
}
