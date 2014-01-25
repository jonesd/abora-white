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
package info.dgjones.abora.white.spaces.basic;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A convenient superclass of all Positions which have no natural ordering.  See
 * UnOrdered::isGE for the defining property of this class.  This class should probably go
 * away and UnOrdered::isGE distributed to the subclasses.
 */
public abstract class UnOrdered extends Position {
	/*
	udanax-top.st:32953:
	Position subclass: #UnOrdered
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:32957:
	UnOrdered comment:
	'A convenient superclass of all Positions which have no natural ordering.  See UnOrdered::isGE for the defining property of this class.  This class should probably go away and UnOrdered::isGE distributed to the subclasses.'!
	*/
	/*
	udanax-top.st:32959:
	(UnOrdered getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected UnOrdered() {
		super();
	}

	protected UnOrdered(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Accessing

	public abstract XnRegion asRegion();
	/*
	udanax-top.st:32964:UnOrdered methodsFor: 'accessing'!
	{XnRegion} asRegion
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:32968:UnOrdered methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//TODOreturn Heaper.takeOop();
		/*
		udanax-top.st:32973:UnOrdered methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	/**
	 * Up in position, isGE is deferred, and isEqual is defined in terms of isEqual.
	 * Here in UnOrdered, we define isGE in terms of isEqual, so we must redefine
	 * isEqual to be deferred.
	 */
	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:32977:UnOrdered methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper} 
		"Up in position, isGE is deferred, and isEqual is defined in terms of isEqual.
		Here in UnOrdered, we define isGE in terms of isEqual, so we must redefine
		isEqual to be deferred."
		
		self subclassResponsibility!
	*/
}
