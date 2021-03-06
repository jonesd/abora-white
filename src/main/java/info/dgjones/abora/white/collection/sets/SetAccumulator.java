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
package info.dgjones.abora.white.collection.sets;

import info.dgjones.abora.white.collection.steppers.Accumulator;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A SetAccumulator accumulates a bunch of objects and then makes an ImmuSet containing all
 * the accumulated objects.  Several people have observed that a SetAccumulator doesn't buy
 * you much because instead you could just store into a MuSet.  While this is true (and is in
 * fact how SetAccumulator is trivially implemented), my feeling is that if what a loop is
 * doing is enumerating a bunch of elements from which a Set is to be formed, using a
 * SetAccumulator in the loops says this more clearly to readers of the code.
 */
public class SetAccumulator extends Accumulator {
	protected MuSet muSet;
	/*
	udanax-top.st:12314:
	Accumulator subclass: #SetAccumulator
		instanceVariableNames: 'muSet {MuSet}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:12318:
	SetAccumulator comment:
	'A SetAccumulator accumulates a bunch of objects and then makes an ImmuSet containing all the accumulated objects.  Several people have observed that a SetAccumulator doesn''t buy you much because instead you could just store into a MuSet.  While this is true (and is in fact how SetAccumulator is trivially implemented), my feeling is that if what a loop is doing is enumerating a bunch of elements from which a Set is to be formed, using a SetAccumulator in the loops says this more clearly to readers of the code.'!
	*/
	/*
	udanax-top.st:12320:
	(SetAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:12362:
	SetAccumulator class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12365:
	(SetAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected SetAccumulator() {
		super();
		muSet = MuSet.make();
		/*
		udanax-top.st:12333:SetAccumulator methodsFor: 'protected: creation'!
		create
			super create.
			muSet _ MuSet make!
		*/
	}

	protected SetAccumulator(ScruSet initialSet) {
		super();
		muSet = initialSet.asMuSet();
		/*
		udanax-top.st:12337:SetAccumulator methodsFor: 'protected: creation'!
		create: initialSet {ScruSet}
			super create.
			muSet _ initialSet asMuSet!
		*/
	}

	protected SetAccumulator(Rcvr receiver) {
		super(receiver);
		muSet = (MuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:12353:SetAccumulator methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			muSet _ receiver receiveHeaper.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	/**
	 * Make a SetAccumulator which starts out with no elements accumulated
	 */
	public static SetAccumulator make() {
		return new SetAccumulator();
		/*
		udanax-top.st:12370:SetAccumulator class methodsFor: 'instance creation'!
		{SetAccumulator} make
			"Make a SetAccumulator which starts out with no elements accumulated"
			^SetAccumulator create!
		*/
	}

	/**
	 * Make a new SetAccumulator in which all the current elements of initialSet are already
	 * accumulated.
	 * Future changes to initialSet have no effect on the accumulator.
	 */
	public static SetAccumulator make(ScruSet initialSet) {
		return new SetAccumulator(initialSet);
		/*
		udanax-top.st:12374:SetAccumulator class methodsFor: 'instance creation'!
		{SetAccumulator} make: initialSet {ScruSet}
			"Make a new SetAccumulator in which all the current elements of initialSet are already accumulated.
			Future changes to initialSet have no effect on the accumulator."
			^SetAccumulator create: initialSet!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public void step(Heaper someObj) {
		muSet.store(someObj);
		/*
		udanax-top.st:12325:SetAccumulator methodsFor: 'accessing'!
		{void} step: someObj {Heaper}
			muSet store: someObj!
		*/
	}

	public Heaper value() {
		return muSet.asImmuSet();
		/*
		udanax-top.st:12328:SetAccumulator methodsFor: 'accessing'!
		{Heaper} value
			^ muSet asImmuSet!
		*/
	}

	/////////////////////////////////////////////
	// Creation

	public Accumulator copy() {
		return new SetAccumulator(muSet.asMuSet());
		/*
		udanax-top.st:12343:SetAccumulator methodsFor: 'creation'!
		{Accumulator} copy
			^ SetAccumulator create: muSet asMuSet!
		*/
	}

//	public ImmuSet get() {
//		passe();
//		/*
//		udanax-top.st:12348:SetAccumulator methodsFor: 'smalltalk: passe'!
//		{ImmuSet} get
//			self passe!
//		*/
//	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(muSet);
		/*
		udanax-top.st:12357:SetAccumulator methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: muSet.!
		*/
	}
}
