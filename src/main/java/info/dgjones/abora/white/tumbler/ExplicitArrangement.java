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

import info.dgjones.abora.white.arrange.Arrangement;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class ExplicitArrangement extends Arrangement {
	protected PtrArray myPositions;
	/*
	udanax-top.st:12585:
	Arrangement subclass: #ExplicitArrangement
		instanceVariableNames: 'myPositions {PtrArray of: Position}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:12589:
	(ExplicitArrangement getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:12670:
	ExplicitArrangement class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12673:
	(ExplicitArrangement getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	public ExplicitArrangement(PtrArray positions) {
		super();
		myPositions = positions;
		/*
		udanax-top.st:12594:ExplicitArrangement methodsFor: 'create'!
		create: positions {PtrArray of: Position}
			super create.
			myPositions := positions.!
		*/
	}

	public IntegerValue indexOf(Position position) {
		for (int i = 0; i < myPositions.count(); i++) {
			if (position.isEqual((myPositions.fetch(i)))) {
				return IntegerValue.make(i);
			}
		}
		throw new AboraRuntimeException(AboraRuntimeException.NOT_FOUND);
		/*
		udanax-top.st:12601:ExplicitArrangement methodsFor: 'accessing'!
		{IntegerVar} indexOf: position {Position}
			Int32Zero almostTo: myPositions count do: [ :i {Int32} |
				(position isEqual: (myPositions fetch: i)) ifTrue:
					[^i]].
			Heaper BLAST: #NotFound.
			^ -1 "compiler fodder"!
		*/
	}

	public IntegerRegion indicesOf(XnRegion region) {
		IntegerRegion result;
		result = IntegerRegion.make();
		for (int i = 0; i < myPositions.count(); i++) {
			if (region.hasMember(((Position) (myPositions.fetch(i))))) {
				result = (IntegerRegion) (result.with(IntegerPos.make(i)));
			}
		}
		return result;
		/*
		udanax-top.st:12609:ExplicitArrangement methodsFor: 'accessing'!
		{IntegerRegion} indicesOf: region {XnRegion}
			| result {IntegerRegion} |
			result := IntegerRegion make.
			Int32Zero almostTo: myPositions count do: [ :i {Int32} |
				(region hasMember: ((myPositions fetch: i) cast: Position)) ifTrue:
					[result := (result with: i integer) cast: IntegerRegion]].
			^result!
		*/
	}

	public XnRegion keysOf(int start, int stop) {
		XnRegion result;
		result = null;
		for (int i = start; i < stop; i++) {
			if (result == null) {
				result = ((Position) (myPositions.fetch(i))).asRegion();
			} else {
				result = result.with(((Position) (myPositions.fetch(i))));
			}
		}
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.INDEX_OUT_OF_BOUNDS);
		}
		return result;
		/*
		udanax-top.st:12618:ExplicitArrangement methodsFor: 'accessing'!
		{XnRegion} keysOf: start {Int32} with: stop {Int32}
			| result {XnRegion} |
			result := NULL.
			start almostTo: stop do: [ :i {Int32} |
				result == NULL ifTrue:
					[result := ((myPositions fetch: i) cast: Position) asRegion]
				ifFalse:
					[result := result with: ((myPositions fetch: i) cast: Position)]].
			result == NULL ifTrue:
				[Heaper BLAST: #IndexOutOfBounds].
			^result!
		*/
	}

	public XnRegion region() {
		XnRegion result;
		result = (XnRegion) (myPositions.get(0));
		for (int i = 1; i < myPositions.count(); i++) {
			result = result.with(((Position) (myPositions.get(i))));
		}
		return result;
		/*
		udanax-top.st:12631:ExplicitArrangement methodsFor: 'accessing'!
		{XnRegion} region
			| result {XnRegion} |
			result := (myPositions get: Int32Zero) cast: XnRegion.
			1 almostTo: myPositions count do: [ :i {Int32} |
				result := result with: ((myPositions get: i) cast: Position)].
			^result!
		*/
	}

	public int actualHashForEqual() {
		return myPositions.contentsHash();
		/*
		udanax-top.st:12641:ExplicitArrangement methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^ myPositions contentsHash!
		*/
	}

	public int hashForEqual() {
		return myPositions.contentsHash();
		/*
		udanax-top.st:12644:ExplicitArrangement methodsFor: 'testing'!
		{UInt32} hashForEqual
			^ myPositions contentsHash!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof ExplicitArrangement) {
			ExplicitArrangement o = (ExplicitArrangement) other;
			return myPositions.contentsEqual(o.positions());
		} else {
			return false;
		}
		/*
		udanax-top.st:12647:ExplicitArrangement methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: ExplicitArrangement
				  into: [:o {ExplicitArrangement} |
				  	^ myPositions contentsEqual: o positions]
				  others: [^ false ].
			^ false "fodder"!
		*/
	}

	public PtrArray positions() {
		return myPositions;
		/*
		udanax-top.st:12656:ExplicitArrangement methodsFor: 'private: accessing'!
		{PtrArray} positions
			^ myPositions!
		*/
	}

	public ExplicitArrangement(Rcvr receiver) {
		super(receiver);
		myPositions = (PtrArray) receiver.receiveHeaper();
		/*
		udanax-top.st:12661:ExplicitArrangement methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myPositions _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myPositions);
		/*
		udanax-top.st:12665:ExplicitArrangement methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myPositions.!
		*/
	}

	public static Arrangement make(PtrArray positions) {
		return new ExplicitArrangement(positions);
		/*
		udanax-top.st:12678:ExplicitArrangement class methodsFor: 'create'!
		{Arrangement} make: positions {PtrArray of: Position} 
			^self create: positions!
		*/
	}
}
