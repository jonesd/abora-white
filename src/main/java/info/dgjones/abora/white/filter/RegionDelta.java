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
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A RegionDelta represents a change in the state of a Region, holding the state before and
 * after some change. They are in some sense complementary to Joints: In the same way that
 * you can use Filters to examine Joints, you can use RegionDeltas to examine Filters. See
 * also Filter::isSwitchedBy(RegionDelta *) and related methods.
 */
public class RegionDelta extends Heaper {
	protected XnRegion myBefore;
	protected XnRegion myAfter;
	/*
	udanax-top.st:42032:
	Heaper subclass: #RegionDelta
		instanceVariableNames: '
			myBefore {XnRegion}
			myAfter {XnRegion}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-filter'!
	*/
	/*
	udanax-top.st:42038:
	RegionDelta comment:
	'A RegionDelta represents a change in the state of a Region, holding the state before and after some change. They are in some sense complementary to Joints: In the same way that you can use Filters to examine Joints, you can use RegionDeltas to examine Filters. See also Filter::isSwitchedBy(RegionDelta *) and related methods.'!
	*/
	/*
	udanax-top.st:42040:
	(RegionDelta getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:42093:
	RegionDelta class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:42096:
	(RegionDelta getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public RegionDelta(XnRegion before, XnRegion after) {
		super();
		myBefore = before;
		myAfter = after;
		/*
		udanax-top.st:42045:RegionDelta methodsFor: 'creation'!
		create: before {XnRegion} with: after {XnRegion}
			super create.
			myBefore _ before.
			myAfter _ after!
		*/
	}

	public int actualHashForEqual() {
		return myBefore.hashForEqual() + myAfter.hashForEqual();
		/*
		udanax-top.st:42052:RegionDelta methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^myBefore hashForEqual + myAfter hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof RegionDelta) {
			RegionDelta rd = (RegionDelta) other;
			return (rd.before().isEqual(myBefore)) && (rd.after().isEqual(myAfter));
		} else {
			return false;
		}
		/*
		udanax-top.st:42055:RegionDelta methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: RegionDelta into: [:rd |
					^(rd before isEqual: myBefore)
					 and: [rd after isEqual: myAfter]]
				others: [^false].
			^false "fodder"!
		*/
	}

	/**
	 * if the before and after are the same
	 */
	public boolean isSame() {
		return myBefore.isEqual(myAfter);
		/*
		udanax-top.st:42064:RegionDelta methodsFor: 'testing'!
		{BooleanVar INLINE} isSame
			"if the before and after are the same"
			^myBefore isEqual: myAfter!
		*/
	}

	/**
	 * The region after the change.
	 */
	public XnRegion after() {
		return myAfter;
		/*
		udanax-top.st:42070:RegionDelta methodsFor: 'accessing'!
		{XnRegion INLINE} after
			"The region after the change."
			
			^myAfter!
		*/
	}

	/**
	 * The region before the change.
	 */
	public XnRegion before() {
		return myBefore;
		/*
		udanax-top.st:42075:RegionDelta methodsFor: 'accessing'!
		{XnRegion INLINE} before
			"The region before the change."
			
			^myBefore!
		*/
	}

	public RegionDelta(Rcvr receiver) {
		super(receiver);
		myBefore = (XnRegion)receiver.receiveHeaper();
		myAfter = (XnRegion)receiver.receiveHeaper();
		/*
		udanax-top.st:42082:RegionDelta methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myBefore _ receiver receiveHeaper.
			myAfter _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myBefore);
		xmtr.sendHeaper(myAfter);
		/*
		udanax-top.st:42087:RegionDelta methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myBefore.
			xmtr sendHeaper: myAfter.!
		*/
	}

	public static Heaper make(XnRegion before, XnRegion after) {
		return new RegionDelta(before, after);
		/*
		udanax-top.st:42101:RegionDelta class methodsFor: 'pseudo constructors'!
		make: before {XnRegion} with: after {XnRegion}
			^RegionDelta create: before with: after!
		*/
	}
}
