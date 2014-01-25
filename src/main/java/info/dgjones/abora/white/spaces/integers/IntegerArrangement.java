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
package info.dgjones.abora.white.spaces.integers;

import java.io.PrintWriter;

import info.dgjones.abora.white.arrange.Arrangement;
import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class IntegerArrangement extends Arrangement {
	protected OrderSpec myOrdering;
	protected IntegerRegion myRegion;
	/*
	udanax-top.st:12681:
	Arrangement subclass: #IntegerArrangement
		instanceVariableNames: '
			myOrdering {OrderSpec}
			myRegion {IntegerRegion}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Integers'!
	*/
	/*
	udanax-top.st:12687:
	(IntegerArrangement getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:12800:
	IntegerArrangement class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12803:
	(IntegerArrangement getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public void copyElements(PrimArray toArray, Dsp toDsp, PrimArray fromArray, Arrangement fromArrange, XnRegion fromRegion) {
		IntegerArrangement other;
		int start;
		int stop;
		int toStart;
		other = (IntegerArrangement) fromArrange;
		if (!(myOrdering.isEqual(other.ordering()))) {
			throw new UnsupportedOperationException();
			//			unimplemented();
		}
		if (!(myRegion.isSimple() && (other.region().isSimple() && (fromRegion.isSimple())))) {
			throw new UnsupportedOperationException();
			//			unimplemented();
		}
		//		TODO knownBug();
		/* Assume ascending for the moment. */
		start = (fromArrange.indexOf((fromRegion.chooseOne(myOrdering)))).asInt32();
		stop = (fromArrange.indexOf((fromRegion.chooseOne(myOrdering.reversed())))).asInt32();
		toStart = (indexOf((toDsp.of((fromRegion.chooseOne(myOrdering)))))).asInt32();
		/* stop < start ifTrue: [| tmp {Int32} | tmp _ start.  start _ stop.  stop _ tmp]. */
		toArray.storeMany(toStart, fromArray, stop + 1 - start, start);
		/*
		udanax-top.st:12692:IntegerArrangement methodsFor: 'accessing'!
		{void} copyElements: toArray {PrimArray} with: toDsp {Dsp}
			with: fromArray {PrimArray} with: fromArrange {Arrangement} with: fromRegion {XnRegion}
			
			| other {IntegerArrangement} start {Int32} stop {Int32} toStart {Int32} |
			other _ fromArrange cast: IntegerArrangement.
			(myOrdering isEqual: other ordering) ifFalse: [self unimplemented].
			(myRegion isSimple and: [other region isSimple and: [fromRegion isSimple]]) ifFalse: [self unimplemented].
			self knownBug.  "Assume ascending for the moment."
			start _ (fromArrange indexOf: (fromRegion chooseOne: myOrdering)) DOTasLong.
			stop _ (fromArrange indexOf: (fromRegion chooseOne: myOrdering reversed)) DOTasLong.
			toStart _ (self indexOf: (toDsp of: (fromRegion chooseOne: myOrdering))) DOTasLong.
			"stop < start ifTrue: [| tmp {Int32} | tmp _ start.  start _ stop.  stop _ tmp]."
			toArray at: toStart
				storeMany: fromArray 
				with: stop + 1 - start
				with: start!
		*/
	}

	/**
	 * Return the index of position into my Region according to my OrderSpec.
	 */
	public IntegerValue indexOf(Position position) {
		IntegerValue sum = IntegerValue.zero();
		IntegerValue intPos = ((IntegerPos) position).asIntegerVar();
		Stepper stepper = myRegion.simpleRegions(myOrdering);
		try {
			IntegerRegion region;
			while ((region = (IntegerRegion) stepper.fetch()) != null) {
				if (region.hasIntMember(intPos)) {
					return sum.plus(intPos.minus(((IntegerPos) region.chooseOne(myOrdering)).asIntegerVar()).abs());
				} else {
					sum = sum.plus(region.count());
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
		/*
		udanax-top.st:12709:IntegerArrangement methodsFor: 'accessing'!
		{IntegerVar} indexOf: position {Position}
			"Return the index of position into my Region according to my OrderSpec."
			| sum {IntegerVar} intPos {IntegerVar} |
			sum _ IntegerVar0.
			intPos _ (position cast: IntegerPos) asIntegerVar.
			(myRegion simpleRegions: myOrdering) forEach: 
				[:region {IntegerRegion} |
				(region hasIntMember: intPos)
					ifTrue: [^sum + (intPos - ((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar) abs]
					ifFalse: [sum _ sum + region count]].
			Heaper BLAST: #NotInTable.
			^ -1 "compiler fodder"!
		*/
	}

	/**
	 * Return the region of all the indices corresponding to positions in region.
	 */
	public IntegerRegion indicesOf(XnRegion region) {
		throw new UnsupportedOperationException();
		//		Someone.shouldImplement();
		//		return null
		//		/* fodder */;
		/*
		udanax-top.st:12723:IntegerArrangement methodsFor: 'accessing'!
		{IntegerRegion} indicesOf: region {XnRegion}
			"Return the region of all the indices corresponding to positions in region."
			Someone shouldImplement.
			^NULL "fodder"!
		*/
	}

	/**
	 * Return the region that corresponds to a range of indices.
	 */
	public XnRegion keysOf(int start, int stop) {
		int offset = start;
		int left = -1;
		Stepper stepper = myRegion.simpleRegions(myOrdering);
		try {
			XnRegion region;
			while ((region = (XnRegion) stepper.fetch()) != null) {
				if (region.count().isLE(IntegerValue.make(offset))) {
					offset = offset - region.count().asInt32();
				} else {
					if (left == -1) {
						left = ((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar().asInt32() + offset;
						offset = stop - (start - offset);
						if (offset <= region.count().asInt32()) {
							return IntegerRegion.make(
								IntegerValue.make(left),
								(((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar().plus(IntegerValue.make(offset))));
						}
					} else {
						int right = ((IntegerPos) (region.chooseOne(myOrdering))).asIntegerVar().asInt32() + offset;
						return IntegerRegion.make(IntegerValue.make(left), IntegerValue.make(right));
					}
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
		/*
		udanax-top.st:12729:IntegerArrangement methodsFor: 'accessing'!
		{XnRegion} keysOf: start {Int32} with: stop {Int32}
			"Return the region that corresponds to a range of indices."
			| offset {Int32} left {Int32} right {Int32} | 
			offset _ start.
			left _ -1.
			(myRegion simpleRegions: myOrdering) forEach: 
				[:region {XnRegion} |
				region count <= offset 
					ifTrue: [offset _ offset - region count DOTasLong]
					ifFalse:
						[left == -1 
							ifTrue: 
								[left _ ((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar DOTasLong + offset.
								offset _ stop - (start - offset).
								offset <= region count DOTasLong ifTrue: 
									[^IntegerRegion make: left 
											with: (((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar + offset)]]
							ifFalse:
								[right _ ((region chooseOne: myOrdering) cast: IntegerPos) asIntegerVar DOTasLong + offset.
								^IntegerRegion make: left with: right]]].
			Heaper BLAST: #NotInTable.
			^ NULL "compiler fodder"!
		*/
	}

	public OrderSpec ordering() {
		return myOrdering;
		/*
		udanax-top.st:12753:IntegerArrangement methodsFor: 'accessing'!
		{OrderSpec} ordering
			^myOrdering!
		*/
	}

	public XnRegion region() {
		return myRegion;
		/*
		udanax-top.st:12756:IntegerArrangement methodsFor: 'accessing'!
		{XnRegion} region
			^myRegion!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(myRegion);
		oo.print(", ");
		oo.print(myOrdering);
		oo.print(")");
		/*
		udanax-top.st:12761:IntegerArrangement methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << myRegion << ', ' << myOrdering << ')'!
		*/
	}

	protected IntegerArrangement(XnRegion region, OrderSpec ordering) {
		super();
		if (!region.isFinite()) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
		}
		myRegion = (IntegerRegion) region;
		myOrdering = ordering;
		/*
		udanax-top.st:12766:IntegerArrangement methodsFor: 'protected: creation'!
		create: region {XnRegion} with: ordering {OrderSpec}
			super create.
			region isFinite ifFalse: [Heaper BLAST: #MustBeFinite].
			myRegion _ region cast: IntegerRegion.
			myOrdering _ ordering!
		*/
	}

	public int actualHashForEqual() {
		return myOrdering.hashForEqual() + myRegion.hashForEqual();
		/*
		udanax-top.st:12774:IntegerArrangement methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^ myOrdering hashForEqual + myRegion hashForEqual!
		*/
	}

	public int hashForEqual() {
		return myOrdering.hashForEqual() + myRegion.hashForEqual();
		/*
		udanax-top.st:12777:IntegerArrangement methodsFor: 'testing'!
		{UInt32} hashForEqual
			^ myOrdering hashForEqual + myRegion hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof IntegerArrangement) {
			IntegerArrangement o = (IntegerArrangement) other;
			return (myOrdering.isEqual(o.ordering())) && (myRegion.isEqual(o.region()));
		} else {
			return false;
		}
		/*
		udanax-top.st:12780:IntegerArrangement methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: IntegerArrangement
				  into: [:o {IntegerArrangement} |
				  	^ (myOrdering isEqual: o ordering) and: [myRegion isEqual: o region]]
				  others: [^ false].
			^ false "fodder"!
		*/
	}

	public IntegerArrangement(Rcvr receiver) {
		super(receiver);
		myOrdering = (OrderSpec) receiver.receiveHeaper();
		myRegion = (IntegerRegion) receiver.receiveHeaper();
		/*
		udanax-top.st:12789:IntegerArrangement methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myOrdering _ receiver receiveHeaper.
			myRegion _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myOrdering);
		xmtr.sendHeaper(myRegion);
		/*
		udanax-top.st:12794:IntegerArrangement methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myOrdering.
			xmtr sendHeaper: myRegion.!
		*/
	}

	public static IntegerArrangement make(XnRegion region, OrderSpec ordering) {
		return new IntegerArrangement(region, ordering);
		/*
		udanax-top.st:12808:IntegerArrangement class methodsFor: 'creation'!
		make: region {XnRegion} with: ordering {OrderSpec} 
			^self create: region with: ordering!
		*/
	}
}
