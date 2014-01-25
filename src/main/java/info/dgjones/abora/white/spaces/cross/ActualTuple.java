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
package info.dgjones.abora.white.spaces.cross;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Default implementation of position in a crossed coordinate space. NOT.A.TYPE
 */
public class ActualTuple extends Tuple {
	protected PtrArray myCoordinates;
	/*
	udanax-top.st:32857:
	Tuple subclass: #ActualTuple
		instanceVariableNames: 'myCoordinates {PtrArray of: Position}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Cross'!
	*/
	/*
	udanax-top.st:32861:
	ActualTuple comment:
	'Default implementation of position in a crossed coordinate space. NOT.A.TYPE'!
	*/
	/*
	udanax-top.st:32863:
	(ActualTuple getOrMakeCxxClassDescription)
		friends:
	'friend class GenericCrossDsp;
	';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:32938:
	ActualTuple class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:32941:
	(ActualTuple getOrMakeCxxClassDescription)
		friends:
	'friend class GenericCrossDsp;
	';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public XnRegion asRegion() {
		PtrArray result;
		result = PtrArray.make(myCoordinates.count());
		for (int i = 0; i < result.count(); i++) {
			result.store(i, (coordinate(i)).asRegion());
		}
		return GenericCrossRegion.make(((CrossSpace) coordinateSpace()), 1, result);
		/*
		udanax-top.st:32871:ActualTuple methodsFor: 'accessing'!
		{XnRegion} asRegion
			
			| result {PtrArray of: XnRegion} |
			result := PtrArray nulls: myCoordinates count.
			Int32Zero almostTo: result count do: [:i {Int32} |
				result at: i store: (self coordinate: i) asRegion].
			^GenericCrossRegion make: (self coordinateSpace cast: CrossSpace) with: 1 with: result!
		*/
	}

	public PtrArray coordinates() {
		return (PtrArray) myCoordinates.copy();
		/*
		udanax-top.st:32879:ActualTuple methodsFor: 'accessing'!
		{PtrArray of: Position} coordinates
			
			^myCoordinates copy cast: PtrArray!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		PtrArray result;
		result = PtrArray.make(myCoordinates.count());
		for (int i = 0; i < result.count(); i++) {
			result.store(i, (coordinate(i)).coordinateSpace());
		}
		return CrossSpace.make(result);
		/*
		udanax-top.st:32883:ActualTuple methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			| result {PtrArray of: CoordinateSpace} |
			result := PtrArray nulls: myCoordinates count.
			Int32Zero almostTo: result count do: [:i {Int32} |
				result at: i store: (self coordinate: i) coordinateSpace].
			^CrossSpace make: result!
		*/
	}

	public int count() {
		return myCoordinates.count();
		/*
		udanax-top.st:32891:ActualTuple methodsFor: 'accessing'!
		{Int32} count
			^ myCoordinates count!
		*/
	}

	public Position positionAt(int dimension) {
		return (Position) (myCoordinates.fetch(dimension));
		/*
		udanax-top.st:32894:ActualTuple methodsFor: 'accessing'!
		{Position} positionAt: dimension {Int32}
			^ (myCoordinates fetch: dimension) cast: Position!
		*/
	}

	public int actualHashForEqual() {
		return myCoordinates.contentsHash();
		/*
		udanax-top.st:32899:ActualTuple methodsFor: 'comparing'!
		{UInt32} actualHashForEqual
			
			^myCoordinates contentsHash!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof ActualTuple) {
			ActualTuple actual = (ActualTuple) other;
			return myCoordinates.contentsEqual(actual.secretCoordinates());
		} else if (other instanceof Tuple) {
			Tuple tuple = (Tuple) other;
			return myCoordinates.contentsEqual(tuple.coordinates());
		} else {
			return false;
		}
		/*
		udanax-top.st:32903:ActualTuple methodsFor: 'comparing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: ActualTuple into: [ :actual |
				^myCoordinates contentsEqual: actual secretCoordinates]
			cast: Tuple into: [ :tuple |
				^myCoordinates contentsEqual: tuple coordinates]
			others:
				[^false].
			^ false "compiler fodder"!
		*/
	}

	public ActualTuple(PtrArray coordinates) {
		super();
		myCoordinates = coordinates;
		/*
		udanax-top.st:32915:ActualTuple methodsFor: 'private: creation'!
		create: coordinates {PtrArray of: Position}
			super create.
			myCoordinates := coordinates!
		*/
	}

	/**
	 * The internal array of coordinates. Do not modify this array!!
	 */
	public PtrArray secretCoordinates() {
		return myCoordinates;
		/*
		udanax-top.st:32922:ActualTuple methodsFor: 'private: accessing'!
		{PtrArray of: Position} secretCoordinates
			"The internal array of coordinates. Do not modify this array!!"
			
			^myCoordinates!
		*/
	}

	public ActualTuple(Rcvr receiver) {
		super(receiver);
		myCoordinates = (PtrArray) receiver.receiveHeaper();
		/*
		udanax-top.st:32929:ActualTuple methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myCoordinates _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myCoordinates);
		/*
		udanax-top.st:32933:ActualTuple methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myCoordinates.!
		*/
	}

	public static Tuple make(PtrArray coordinates) {
		return new ActualTuple(coordinates);
		/*
		udanax-top.st:32949:ActualTuple class methodsFor: 'pseudoconstructors'!
		{Tuple} make: coordinates {PtrArray of: Position}
			^self create: coordinates!
		*/
	}
}
