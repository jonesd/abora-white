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
 * Like a SetAccumulator, a UnionRecruiter makes an ImmuSet out of the things that it
 * Accumulates.  However, the things that a UnionRecruiter accumulates must themselves be
 * ScruSets, and the resulting ImmuSet consists of the union of the elements of each of the
 * accumulated sets as of the time they were accumulated.
 */
public class UnionRecruiter extends Accumulator {
	protected MuSet muSet;
	/*
	udanax-top.st:12474:
	Accumulator subclass: #UnionRecruiter
		instanceVariableNames: 'muSet {MuSet}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:12478:
	UnionRecruiter comment:
	'Like a SetAccumulator, a UnionRecruiter makes an ImmuSet out of the things that it Accumulates.  However, the things that a UnionRecruiter accumulates must themselves be ScruSets, and the resulting ImmuSet consists of the union of the elements of each of the accumulated sets as of the time they were accumulated.'!
	*/
	/*
	udanax-top.st:12480:
	(UnionRecruiter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:12516:
	UnionRecruiter class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12519:
	(UnionRecruiter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected UnionRecruiter() {
		super();
		muSet = MuSet.make();
		/*
		udanax-top.st:12493:UnionRecruiter methodsFor: 'protected: creation'!
		create
			super create.
			muSet _ MuSet make!
		*/
	}

	protected UnionRecruiter(Rcvr receiver) {
		super(receiver);
		muSet = (MuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:12507:UnionRecruiter methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			muSet _ receiver receiveHeaper.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	/**
	 * Make a new UnionRecruiter which hasn't yet accumulated anything
	 */
	public static UnionRecruiter make() {
		return new UnionRecruiter();
		/*
		udanax-top.st:12524:UnionRecruiter class methodsFor: 'pseudo constructors'!
		{UnionRecruiter} make
			"Make a new UnionRecruiter which hasn't yet accumulated anything"
			^UnionRecruiter create!
		*/
	}
	
	/////////////////////////////////////////////
	// Accessing

	public void step(Heaper someObj) {
		muSet.storeAll((ScruSet) someObj);
		/*
		udanax-top.st:12485:UnionRecruiter methodsFor: 'accessing'!
		{void} step: someObj {Heaper}
			muSet storeAll: (someObj cast: ScruSet)!
		*/
	}

	public Heaper value() {
		return muSet.asImmuSet();
		/*
		udanax-top.st:12488:UnionRecruiter methodsFor: 'accessing'!
		{Heaper} value
			^ muSet asImmuSet!
		*/
	}

	/////////////////////////////////////////////
	// Creation

	public Accumulator copy() {
		Accumulator result = UnionRecruiter.make();
		result.step(muSet);
		return result;
		/*
		udanax-top.st:12499:UnionRecruiter methodsFor: 'creation'!
		{Accumulator} copy
			| result {Accumulator} |
			result _ UnionRecruiter make.
			result step: muSet.
			^result!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(muSet);
		/*
		udanax-top.st:12511:UnionRecruiter methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: muSet.!
		*/
	}
}
