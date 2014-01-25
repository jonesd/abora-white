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
package info.dgjones.abora.white.arrange;

import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Generally represents a pair of an OrderSpec and a Region.  Arrangements map between
 * regions and primArrays.
 */
public abstract class Arrangement extends Heaper {
	/*
	udanax-top.st:12528:
	Heaper subclass: #Arrangement
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-arrange'!
	*/
	/*
	udanax-top.st:12532:
	Arrangement comment:
	'Generally represents a pair of an OrderSpec and a Region.  Arrangements map between regions and primArrays.'!
	*/
	/*
	udanax-top.st:12534:
	(Arrangement getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; add: #COPY; yourself)!
	*/

	protected Arrangement() {
		super();
	}

	/**
	 * Copy elements into toArray arranged according to the receiver.
	 * Copy them from fromArray arranged according to fromArrange.
	 * The source region is fromRegion.  It gets tranformed by toDsp
	 * into the toArray.
	 */
	public void copyElements(PrimArray toArray, Dsp toDsp, PrimArray fromArray, Arrangement fromArrange, XnRegion fromRegion) {
		Stepper stepper = fromRegion.stepper();
		try {
			Position key;
			while ((key = (Position) stepper.fetch()) != null) {
				toArray.storeValue((indexOf((toDsp.of(key)))).asInt32(), (fromArray.fetchValue((fromArrange.indexOf(key)).asInt32())));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		/*
		udanax-top.st:12539:Arrangement methodsFor: 'accessing'!
		{void} copyElements: toArray {PrimArray} with: toDsp {Dsp}
			with: fromArray {PrimArray} with: fromArrange {Arrangement} with: fromRegion {XnRegion}
			
			"Copy elements into toArray arranged according to the receiver. 
			 Copy them from fromArray arranged according to fromArrange.  
			 The source region is fromRegion.  It gets tranformed by toDsp
			 into the toArray."
			
			fromRegion stepper forEach: 
				[:key {Position} |
				toArray at: (self indexOf: (toDsp of: key)) DOTasLong
					storeValue: (fromArray fetchValue: (fromArrange indexOf: key) DOTasLong)]!
		*/
	}

	/**
	 * Return the index of position into my Region according to my OrderSpec.
	 */
	public abstract IntegerValue indexOf(Position position);
	/*
	udanax-top.st:12552:Arrangement methodsFor: 'accessing'!
	{IntegerVar} indexOf: position {Position unused}
		"Return the index of position into my Region according to my OrderSpec."
		self subclassResponsibility!
	*/

	/**
	 * Return the region of all the indices corresponding to positions in region.
	 */
	public abstract IntegerRegion indicesOf(XnRegion region);
	/*
	udanax-top.st:12557:Arrangement methodsFor: 'accessing'!
	{IntegerRegion} indicesOf: region {XnRegion}
		"Return the region of all the indices corresponding to positions in region."
		self subclassResponsibility!
	*/

	/**
	 * Return the region that corresponds to a range of indices.
	 */
	public abstract XnRegion keysOf(int start, int stop);
	/*
	udanax-top.st:12562:Arrangement methodsFor: 'accessing'!
	{XnRegion} keysOf: start {Int32} with: stop {Int32}
		"Return the region that corresponds to a range of indices."
		self subclassResponsibility!
	*/

	/**
	 * The region of positions in the arrangement
	 */
	public abstract XnRegion region();
	/*
	udanax-top.st:12567:Arrangement methodsFor: 'accessing'!
	{XnRegion} region
		"The region of positions in the arrangement"
		
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		return Heaper.takeOop();
		/*
		udanax-top.st:12574:Arrangement methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	public Arrangement(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:12579:Arrangement methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:12582:Arrangement methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
