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
package info.dgjones.abora.white.filter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.unordered.IdentityDsp;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * There are no non-trivial Dsps currently defined on a FilterSpace.
 * It would be possible to define them with reference to a Dsp in the baseSpace, as
 * filterDsp->of(filter)->match(R) iff filter->match(filterDsp->baseDsp()->inverseOf(R))
 * for all R in the base space.
 * <p>
 * However, we have not yet found a use for them.
 */
public class FilterDsp extends IdentityDsp {
	protected FilterSpace myCS;

	/*
	udanax-top.st:29569:
	IdentityDsp subclass: #FilterDsp
		instanceVariableNames: 'myCS {FilterSpace}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Filter'!
	*/
	/*
	udanax-top.st:29573:
	FilterDsp comment:
	'There are no non-trivial Dsps currently defined on a FilterSpace.
	It would be possible to define them with reference to a Dsp in the baseSpace, as
		filterDsp->of(filter)->match(R) iff filter->match(filterDsp->baseDsp()->inverseOf(R))
			for all R in the base space.
	However, we have not yet found a use for them.'!
	*/
	/*
	udanax-top.st:29580:
	(FilterDsp getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:29619:
	FilterDsp class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:29622:
	(FilterDsp getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	protected FilterDsp(CoordinateSpace cs) {
		super();
		myCS = (FilterSpace) cs;
		/*
		udanax-top.st:29585:FilterDsp methodsFor: 'creation'!
		create: cs {CoordinateSpace}
			super create.
			myCS _ cs cast: FilterSpace.!
		*/
	}

	public int actualHashForEqual() {
		return myCS.hashForEqual() + getClass().hashCode();
		/*
		udanax-top.st:29591:FilterDsp methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^myCS hashForEqual + #cat.U.FilterDsp hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof FilterDsp) {
			FilterDsp fd = (FilterDsp) other;
			return fd.coordinateSpace().isEqual(myCS);
		} else {
			return false;
		}
		/*
		udanax-top.st:29594:FilterDsp methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: FilterDsp into: [:fd |
					^fd coordinateSpace isEqual: myCS]
				others: [^false].
			^false "fodder"!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return myCS;
		/*
		udanax-top.st:29604:FilterDsp methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^myCS!
		*/
	}

	protected FilterDsp(Rcvr receiver) {
		super(receiver);
		myCS = (FilterSpace) receiver.receiveHeaper();
		/*
		udanax-top.st:29610:FilterDsp methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myCS _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myCS);
		/*
		udanax-top.st:29614:FilterDsp methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myCS.!
		*/
	}

	/**
	 * An identity Dsp on the given FilterSpace.
	 */
	public static FilterDsp make(FilterSpace cs) {
		return new FilterDsp(cs);
		/*
		udanax-top.st:29627:FilterDsp class methodsFor: 'pseudo constructors'!
		make: cs {FilterSpace}
			"An identity Dsp on the given FilterSpace."
			
			^FilterDsp create: cs!
		*/
	}

//	public static void suppressInitTimeInherited() {
//		/*
//		udanax-top.st:29634:FilterDsp class methodsFor: 'smalltalk: initialization'!
//		suppressInitTimeInherited!
//		*/
//	}

//	public static void suppressLinkTimeInherited() {
//		/*
//		udanax-top.st:29636:FilterDsp class methodsFor: 'smalltalk: initialization'!
//		suppressLinkTimeInherited!
//		*/
//	}
}
