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
package info.dgjones.abora.white.cross;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.Mapping;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.cross.ActualTuple;
import info.dgjones.abora.white.spaces.cross.BoxStepper;
import info.dgjones.abora.white.spaces.cross.CrossSpace;
import info.dgjones.abora.white.spaces.cross.GenericCrossRegion;
import info.dgjones.abora.white.spaces.cross.GenericCrossSpace;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Was NOT.A.TYPE but that obstructed compilation.
 */
public class GenericCrossDsp extends CrossMapping {
	protected CrossSpace mySpace;
	protected PtrArray mySubDsps;
	/*
	udanax-top.st:29265:
	CrossMapping subclass: #GenericCrossDsp
		instanceVariableNames: '
			mySpace {CrossSpace}
			mySubDsps {PtrArray of: Dsp}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-cross'!
	*/
	/*
	udanax-top.st:29271:
	GenericCrossDsp comment:
	' Was NOT.A.TYPE but that obstructed compilation.'!
	*/
	/*
	udanax-top.st:29273:
	(GenericCrossDsp getOrMakeCxxClassDescription)
		friends:
	'friend class GenericCrossSpace;
	';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:29434:
	GenericCrossDsp class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:29437:
	(GenericCrossDsp getOrMakeCxxClassDescription)
		friends:
	'friend class GenericCrossSpace;
	';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public CoordinateSpace coordinateSpace() {
		return mySpace;
		/*
		udanax-top.st:29281:GenericCrossDsp methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^mySpace!
		*/
	}

	public boolean isIdentity() {
		for (int i = 0; i < mySubDsps.count(); i++) {
			if (!(subMapping(i)).isIdentity()) {
				return false;
			}
		}
		return true;
		/*
		udanax-top.st:29285:GenericCrossDsp methodsFor: 'accessing'!
		{BooleanVar} isIdentity
			
			Int32Zero almostTo: mySubDsps count do: [:i {Int32} |
				(self subMapping: i) isIdentity ifFalse:
					[^false]].
			^true!
		*/
	}

	public Dsp subMapping(int index) {
		return (Dsp) (mySubDsps.fetch(index));
		/*
		udanax-top.st:29292:GenericCrossDsp methodsFor: 'accessing'!
		{Dsp} subMapping: index {Int32}
			^(mySubDsps fetch: index) cast: Dsp!
		*/
	}

	public PtrArray subMappings() {
		return (PtrArray) mySubDsps.copy();
		/*
		udanax-top.st:29296:GenericCrossDsp methodsFor: 'accessing'!
		{PtrArray of: Dsp} subMappings
			^mySubDsps copy cast: PtrArray!
		*/
	}

	public GenericCrossDsp(CrossSpace space, PtrArray subDsps) {
		super();
		mySpace = space;
		mySubDsps = subDsps;
		/*
		udanax-top.st:29302:GenericCrossDsp methodsFor: 'private: creation'!
		create: space {CrossSpace} with: subDsps {PtrArray of: Dsp}
			
			super create.
			mySpace := space.
			mySubDsps := subDsps!
		*/
	}

	public Position inverseOf(Position position) {
		if (position instanceof ActualTuple) {
			ActualTuple tuple = (ActualTuple) position;
			PtrArray result;
			result = PtrArray.make(tuple.count());
			for (int dimension = 0; dimension < tuple.count(); dimension++) {
				result.store(dimension, ((subMapping(dimension)).inverseOf((tuple.positionAt(dimension)))));
			}
			return ActualTuple.make(result);
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29310:GenericCrossDsp methodsFor: 'transforming'!
		{Position} inverseOf: position {Position}
			position cast: ActualTuple into: [ :tuple | | result {PtrArray of: Position} |
				result := PtrArray nulls: tuple count.
				Int32Zero almostTo: tuple count do: [ :dimension {Int32} |
					result at: dimension
						store: ((self subMapping: dimension) inverseOf: (tuple positionAt: dimension))].
				^ActualTuple make: result].
			^ NULL "compiler fodder"!
		*/
	}

	public XnRegion inverseOfAll(XnRegion region) {
		if (region instanceof GenericCrossRegion) {
			GenericCrossRegion cross = (GenericCrossRegion) region;
			BoxAccumulator result;
			BoxStepper boxes;
			result = BoxAccumulator.make(mySpace, cross.boxCount());
			boxes = cross.boxStepper();
			while (boxes.hasValue()) {
				result.addInverseTransformedBox(boxes, this);
				boxes.step();
			}
			return result.region();
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29320:GenericCrossDsp methodsFor: 'transforming'!
		{XnRegion} inverseOfAll: region {XnRegion}
			
			region cast: GenericCrossRegion into:
				[ :cross | | result {BoxAccumulator} boxes {BoxStepper} |
				result := BoxAccumulator make: mySpace with: cross boxCount.
				boxes := cross boxStepper.
				[boxes hasValue] whileTrue:
					[result addInverseTransformedBox: boxes with: self.
					boxes step].
				^result region].
			^ NULL "compiler fodder"!
		*/
	}

	public Position of(Position position) {
		if (position instanceof ActualTuple) {
			ActualTuple tuple = (ActualTuple) position;
			PtrArray result;
			result = PtrArray.make(tuple.count());
			for (int dimension = 0; dimension < tuple.count(); dimension++) {
				result.store(dimension, ((subMapping(dimension)).of((tuple.positionAt(dimension)))));
			}
			return ActualTuple.make(result);
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29332:GenericCrossDsp methodsFor: 'transforming'!
		{Position} of: position {Position}
			position cast: ActualTuple into: [ :tuple | | result {PtrArray of: Position} |
				result := PtrArray nulls: tuple count.
				Int32Zero almostTo: tuple count do: [ :dimension {Int32} |
					result at: dimension
						store: ((self subMapping: dimension) of: (tuple positionAt: dimension))].
				^ActualTuple make: result].
			^ NULL "compiler fodder"!
		*/
	}

	public XnRegion ofAll(XnRegion region) {
		if (region instanceof GenericCrossRegion) {
			GenericCrossRegion cross = (GenericCrossRegion) region;
			BoxAccumulator result;
			BoxStepper boxes;
			result = BoxAccumulator.make(mySpace, cross.boxCount());
			boxes = cross.boxStepper();
			while (boxes.hasValue()) {
				result.addTransformedBox(boxes, this);
				boxes.step();
			}
			return result.region();
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29342:GenericCrossDsp methodsFor: 'transforming'!
		{XnRegion} ofAll: region {XnRegion}
			
			region cast: GenericCrossRegion into:
				[ :cross | | result {BoxAccumulator} boxes {BoxStepper} |
				result := BoxAccumulator make: mySpace with: cross boxCount.
				boxes := cross boxStepper.
				[boxes hasValue] whileTrue:
					[result addTransformedBox: boxes with: self.
					boxes step].
				^result region].
			^ NULL "compiler fodder"!
		*/
	}

	public Dsp compose(Dsp other) {
		PtrArray newSubDsps;
		newSubDsps = PtrArray.make(mySubDsps.count());
		if (other instanceof CrossMapping) {
			CrossMapping cross = (CrossMapping) other;
			for (int dimension = 0; dimension < newSubDsps.count(); dimension++) {
				newSubDsps.store(dimension, ((subMapping(dimension)).compose((cross.subMapping(dimension)))));
			}
			return GenericCrossDsp.make(mySpace, newSubDsps);
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29356:GenericCrossDsp methodsFor: 'combining'!
		{Dsp} compose: other {Dsp}
			
			| newSubDsps {PtrArray of: Dsp} |
			newSubDsps := PtrArray nulls: mySubDsps count.
			other cast: CrossMapping into: [ :cross |
				Int32Zero almostTo: newSubDsps count do: [ :dimension {Int32} |
					newSubDsps at: dimension
						store: ((self subMapping: dimension) compose: (cross subMapping: dimension))].
				^GenericCrossDsp make: mySpace with: newSubDsps].
			^ NULL "compiler fodder"!
		*/
	}

	public Mapping inverse() {
		PtrArray newSubDsps;
		newSubDsps = PtrArray.make(mySubDsps.count());
		for (int dimension = 0; dimension < newSubDsps.count(); dimension++) {
			newSubDsps.store(dimension, ((Dsp) (subMapping(dimension)).inverse()));
		}
		return new GenericCrossDsp(mySpace, newSubDsps);
		/*
		udanax-top.st:29367:GenericCrossDsp methodsFor: 'combining'!
		{Mapping} inverse
			
			| newSubDsps {PtrArray of: Dsp} |
			newSubDsps := PtrArray nulls: mySubDsps count.
			Int32Zero almostTo: newSubDsps count do: [ :dimension {Int32} |
				newSubDsps at: dimension
					store: ((self subMapping: dimension) inverse cast: Dsp)].
			^GenericCrossDsp create: mySpace with: newSubDsps!
		*/
	}

	public Dsp inverseCompose(Dsp other) {
		PtrArray newSubDsps;
		newSubDsps = PtrArray.make(mySubDsps.count());
		if (other instanceof CrossMapping) {
			CrossMapping cross = (CrossMapping) other;
			for (int dimension = 0; dimension < newSubDsps.count(); dimension++) {
				newSubDsps.store(dimension, ((subMapping(dimension)).inverseCompose((cross.subMapping(dimension)))));
			}
			return GenericCrossDsp.make(mySpace, newSubDsps);
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29376:GenericCrossDsp methodsFor: 'combining'!
		{Dsp} inverseCompose: other {Dsp}
			
			| newSubDsps {PtrArray of: Dsp} |
			newSubDsps := PtrArray nulls: mySubDsps count.
			other cast: CrossMapping into: [ :cross |
				Int32Zero almostTo: newSubDsps count do: [ :dimension {Int32} |
					newSubDsps at: dimension
						store: ((self subMapping: dimension) inverseCompose: (cross subMapping: dimension))].
				^GenericCrossDsp make: mySpace with: newSubDsps].
			^ NULL "compiler fodder"!
		*/
	}

	public Dsp minus(Dsp other) {
		PtrArray newSubDsps;
		newSubDsps = PtrArray.make(mySubDsps.count());
		if (other instanceof CrossMapping) {
			CrossMapping cross = (CrossMapping) other;
			for (int dimension = 0; dimension < newSubDsps.count(); dimension++) {
				newSubDsps.store(dimension, ((subMapping(dimension)).minus((cross.subMapping(dimension)))));
			}
			return GenericCrossDsp.make(mySpace, newSubDsps);
		}
		return null
		/* compiler fodder */;
		/*
		udanax-top.st:29387:GenericCrossDsp methodsFor: 'combining'!
		{Dsp} minus: other {Dsp}
			
			| newSubDsps {PtrArray of: Dsp} |
			newSubDsps := PtrArray nulls: mySubDsps count.
			other cast: CrossMapping into: [ :cross |
				Int32Zero almostTo: newSubDsps count do: [ :dimension {Int32} |
					newSubDsps at: dimension
						store: ((self subMapping: dimension) minus: (cross subMapping: dimension))].
				^GenericCrossDsp make: mySpace with: newSubDsps].
			^ NULL "compiler fodder"!
		*/
	}

	/**
	 * The actual array of sub Dsps. DO NOT MODIFY
	 */
	public PtrArray secretSubDsps() {
		return mySubDsps;
		/*
		udanax-top.st:29400:GenericCrossDsp methodsFor: 'private: accessing'!
		{PtrArray of: Dsp} secretSubDsps
			"The actual array of sub Dsps. DO NOT MODIFY"
			
			^mySubDsps!
		*/
	}

	public int actualHashForEqual() {
		return (mySpace.hashForEqual() ^ mySubDsps.contentsHash()) ^ getClass().hashCode();
		/*
		udanax-top.st:29407:GenericCrossDsp methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^(mySpace hashForEqual
				bitXor: mySubDsps contentsHash)
				bitXor: self getCategory hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof GenericCrossDsp) {
			GenericCrossDsp cross = (GenericCrossDsp) other;
			return mySubDsps.contentsEqual(cross.secretSubDsps());
		} else {
			return false;
		}
		/*
		udanax-top.st:29413:GenericCrossDsp methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: GenericCrossDsp into: [ :cross |
				^mySubDsps contentsEqual: cross secretSubDsps]
			others:
				[^false].
			^ false "compiler fodder"!
		*/
	}

	public GenericCrossDsp(Rcvr receiver) {
		super(receiver);
		mySpace = (CrossSpace) receiver.receiveHeaper();
		mySubDsps = (PtrArray) receiver.receiveHeaper();
		/*
		udanax-top.st:29423:GenericCrossDsp methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			mySpace _ receiver receiveHeaper.
			mySubDsps _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(mySpace);
		xmtr.sendHeaper(mySubDsps);
		/*
		udanax-top.st:29428:GenericCrossDsp methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: mySpace.
			xmtr sendHeaper: mySubDsps.!
		*/
	}

//	public static Heaper make(Object space) {
//		return make(space, null);
//		/*
//		udanax-top.st:29445:GenericCrossDsp class methodsFor: 'smalltalk: defaults'!
//		make: space
//			^self make: space with: NULL!
//		*/
//	}

	/**
	 * Only used during construction; must pass the array in explicitly since the space isnt
	 * initialized yet
	 */
	public static GenericCrossDsp identity(GenericCrossSpace space, PtrArray subSpaces) {
		PtrArray result;
		result = PtrArray.make(subSpaces.count());
		for (int dimension = 0; dimension < result.count(); dimension++) {
			result.store(dimension, ((CoordinateSpace) (subSpaces.fetch(dimension))).identityDsp());
		}
		return new GenericCrossDsp(space, result);
		/*
		udanax-top.st:29451:GenericCrossDsp class methodsFor: 'private: pseudoconstructors'!
		{GenericCrossDsp} identity: space {GenericCrossSpace}
			with: subSpaces {PtrArray of: CoordinateSpace}
			"Only used during construction; must pass the array in explicitly since the space isnt initialized yet"
			| result {PtrArray of: Dsp} |
			result := PtrArray nulls: subSpaces count.
			Int32Zero almostTo: result count do: [ :dimension {Int32} |
				result at: dimension store: ((subSpaces fetch: dimension) cast: CoordinateSpace) identityDsp].
			^self create: space with: result!
		*/
	}
}
