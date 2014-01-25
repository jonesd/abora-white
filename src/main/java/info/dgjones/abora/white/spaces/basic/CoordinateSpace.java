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

import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A coordinate space represents (among other things) the domain space of a table.
 * Corresponding to each coordinate space will be a set of objects of the following kinds:
 * <p>
 * Position -- The elements of the coordinate space.<br>
 * Mapping -- (Add a description.)<br>
 * OrderSpec -- The ways of specifying partial orders of this coordinate space's Positions.<br>
 * XuRegion -- An XuRegion represents a set of Positions.  The domain of a table is an
 * XuRegion.
 * <p>
 * When defining a new coordinate space class, one generally defines new corresponing
 * subclasses of each of the above classes.  A kind of any of the above classes knows what
 * coordinate space it is a part of (the "coordinateSpace()" message will yield an
 * appropriate kind of CoordinateSpace).  CoordinateSpace objects exist mostly just to
 * represent this commonality.  Coordinate spaces are disjoint--it is an error to use any of
 * the generic protocol of any of the above classes if the objects in question are of two
 * different coordinate spaces.  For example, "<code>dsp->of (pos)</code>" is not an error iff
 * "<code>dsp->coordinateSpace()->isEqual (pos->coordinateSpace())</code>".
 * <p>
 * Note that this class is not COPY or even PSEUDO_COPY.  All of the instance variables for
 * CoordinateSpace are basically cached
 * quantities that require vary little actual state from the derived classes in order to be
 * constructed.  This realization allows a knot
 * to be untangled when reading these objects from external storage.
 */
public abstract class CoordinateSpace extends Heaper {
	protected XnRegion myEmptyRegion;
	protected XnRegion myFullRegion;
	protected Dsp myIdentityDsp;
	protected OrderSpec myAscending;
	protected OrderSpec myDescending;
	/*
	udanax-top.st:14289:
	Heaper subclass: #CoordinateSpace
		instanceVariableNames: '
			myEmptyRegion {XnRegion}
			myFullRegion {XnRegion}
			myIdentityDsp {Dsp}
			myAscending {OrderSpec | NULL}
			myDescending {OrderSpec | NULL}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:14298:
	CoordinateSpace comment:
	'A coordinate space represents (among other things) the domain space of a table. Corresponding to each coordinate space will be a set of objects of the following kinds:
		
		Position -- The elements of the coordinate space.
		Mapping -- (Add a description.)
		OrderSpec -- The ways of specifying partial orders of this coordinate space''s Positions.
		XuRegion -- An XuRegion represents a set of Positions.  The domain of a table is an XuRegion.
		
		When defining a new coordinate space class, one generally defines new corresponing subclasses of each of the above classes.  A kind of any of the above classes knows what coordinate space it is a part of (the "coordinateSpace()" message will yield an appropriate kind of CoordinateSpace).  CoordinateSpace objects exist mostly just to represent this commonality.  Coordinate spaces are disjoint--it is an error to use any of the generic protocol of any of the above classes if the objects in question are of two different coordinate spaces.  For example, "dsp->of (pos)" is not an error iff "dsp->coordinateSpace()->isEqual (pos->coordinateSpace())".
		
	Note that this class is not COPY or even PSEUDO_COPY.  All of the instance variables for CoordinateSpace are basically cached
	quantities that require vary little actual state from the derived classes in order to be constructed.  This realization allows a knot
	to be untangled when reading these objects from external storage.'!
	*/
	/*
	udanax-top.st:14311:
	(CoordinateSpace getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:14486:
	CoordinateSpace class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:14489:
	(CoordinateSpace getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		TODO return Heaper.takeOop();
		/*
		udanax-top.st:14316:CoordinateSpace methodsFor: 'accessing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	/**
	 * Essential.  The natural full-ordering of the coordinate space.
	 */
	public OrderSpec ascending() {
		return getAscending();
		/*
		udanax-top.st:14320:CoordinateSpace methodsFor: 'accessing'!
		{OrderSpec CLIENT INLINE} ascending
			"Essential.  The natural full-ordering of the coordinate space."
			
			^self getAscending!
		*/
	}

	/**
	 * Essential. A Mapping which maps each position in this space to every position in the range
	 * region. The region can be from any CoordinateSpace.
	 */
	public Mapping completeMapping(XnRegion range) {
		return Mapping.make(this, range);
		/*
		udanax-top.st:14325:CoordinateSpace methodsFor: 'accessing'!
		{Mapping CLIENT INLINE} completeMapping: range {XnRegion}
			"Essential. A Mapping which maps each position in this space to every position in the range region. The region can be from any CoordinateSpace."
			
			^Mapping make.CoordinateSpace: self with.Region: range!
		*/
	}

	/**
	 * The mirror image of the partial order returned by 'CoordinateSpace::ascending'.
	 */
	public OrderSpec descending() {
		return getDescending();
		/*
		udanax-top.st:14330:CoordinateSpace methodsFor: 'accessing'!
		{OrderSpec CLIENT INLINE} descending
			"The mirror image of the partial order returned by 'CoordinateSpace::ascending'."
			
			^self getDescending!
		*/
	}

	/**
	 * Essential.  An empty region in this coordinate space
	 */
	public XnRegion emptyRegion() {
		return myEmptyRegion;
		/*
		udanax-top.st:14335:CoordinateSpace methodsFor: 'accessing'!
		{XnRegion CLIENT INLINE} emptyRegion
			"Essential.  An empty region in this coordinate space"
			
			^myEmptyRegion!
		*/
	}

	/**
	 * The natural full-ordering of the coordinate space.
	 */
	public OrderSpec fetchAscending() {
		return myAscending;
		/*
		udanax-top.st:14340:CoordinateSpace methodsFor: 'accessing'!
		{(OrderSpec | NULL) INLINE} fetchAscending
			"The natural full-ordering of the coordinate space."
			^myAscending!
		*/
	}

	/**
	 * The mirror image of the partial order returned by
	 * 'CoordinateSpace::fetchAscending'.
	 */
	public OrderSpec fetchDescending() {
		return myDescending;
		/*
		udanax-top.st:14345:CoordinateSpace methodsFor: 'accessing'!
		{(OrderSpec | NULL) INLINE} fetchDescending
			"The mirror image of the partial order returned by 
			'CoordinateSpace::fetchAscending'."
			^myDescending!
		*/
	}

	/**
	 * A full region in this coordinate space
	 */
	public XnRegion fullRegion() {
		return myFullRegion;
		/*
		udanax-top.st:14351:CoordinateSpace methodsFor: 'accessing'!
		{XnRegion CLIENT INLINE} fullRegion
			"A full region in this coordinate space"
			
			^myFullRegion!
		*/
	}

	/**
	 * Essential.  The natural full-ordering of the coordinate space.
	 */
	public OrderSpec getAscending() {
		OrderSpec result = fetchAscending();
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NO_FULL_ORDER);
		}
		return result;
		/*
		udanax-top.st:14356:CoordinateSpace methodsFor: 'accessing'!
		{OrderSpec} getAscending
			"Essential.  The natural full-ordering of the coordinate space."
			
			| result {OrderSpec | NULL} |
			result := self fetchAscending.
			result == NULL ifTrue:
				[Heaper BLAST: #NoFullOrder].
			^result!
		*/
	}

	/**
	 * The mirror image of the partial order returned by 'CoordinateSpace::getAscending'.
	 */
	public OrderSpec getDescending() {
		OrderSpec result = fetchDescending();
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NO_FULL_ORDER);
		}
		return result;
		/*
		udanax-top.st:14365:CoordinateSpace methodsFor: 'accessing'!
		{OrderSpec} getDescending
			"The mirror image of the partial order returned by 'CoordinateSpace::getAscending'."
			
			| result {OrderSpec | NULL} |
			result := self fetchDescending.
			result == NULL ifTrue:
				[Heaper BLAST: #NoFullOrder].
			^result!
		*/
	}

	/**
	 * A Dsp which maps all positions in the coordinate space onto themselves
	 */
	public Dsp identityDsp() {
		return myIdentityDsp;
		/*
		udanax-top.st:14374:CoordinateSpace methodsFor: 'accessing'!
		{Dsp INLINE} identityDsp
			"A Dsp which maps all positions in the coordinate space onto themselves"
			
			^myIdentityDsp!
		*/
	}

	/**
	 * Essential.  A Mapping which maps all positions in the coordinate space onto themselves
	 */
	public Mapping identityMapping() {
		return identityDsp();
		/*
		udanax-top.st:14379:CoordinateSpace methodsFor: 'accessing'!
		{Mapping CLIENT INLINE} identityMapping
			"Essential.  A Mapping which maps all positions in the coordinate space onto themselves"
			
			^self identityDsp!
		*/
	}

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:14384:CoordinateSpace methodsFor: 'accessing'!
	{BooleanVar} isEqual: other{Heaper}
		self subclassResponsibility!
	*/

	/**
	 * tell whether this is a valid Position/XuRegion/Dsp/OrderSpec for this space
	 */
	public boolean verify(Heaper thing) {
		if (thing instanceof Position) {
			Position position = (Position) thing;
			return isEqual(position.coordinateSpace());
		} else if (thing instanceof XnRegion) {
			XnRegion region = (XnRegion) thing;
			return isEqual(region.coordinateSpace());
		} else if (thing instanceof Dsp) {
			Dsp dsp = (Dsp) thing;
			return isEqual(dsp.coordinateSpace());
		} else if (thing instanceof OrderSpec) {
			OrderSpec orderSpec = (OrderSpec) thing;
			return isEqual(orderSpec.coordinateSpace());			
		} else {
			throw new IllegalArgumentException();
		}
		/*
		udanax-top.st:14388:CoordinateSpace methodsFor: 'accessing'!
		{BooleanVar} verify: thing {Heaper}
			"tell whether this is a valid Position/XuRegion/Dsp/OrderSpec for this space"
			thing cast: (Position | XnRegion | Dsp | OrderSpec) into: [:t |
					^self isEqual: t coordinateSpace].
			"cast into blasts here."
			^false!
		*/
	}

	protected CoordinateSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp) {
		this(emptyRegion, fullRegion, identityDsp, null, null);
		/*
		udanax-top.st:14398:CoordinateSpace methodsFor: 'smalltalk: defaults'!
		create: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			
			self create: emptyRegion with: fullRegion with: identityDsp with: NULL with: NULL!
		*/
	}

	protected CoordinateSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending) {
		this(emptyRegion, fullRegion, identityDsp, ascending, null);
		/*
		udanax-top.st:14404:CoordinateSpace methodsFor: 'smalltalk: defaults'!
		create: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			with: ascending {OrderSpec default: NULL}
			
			self create: emptyRegion with: fullRegion with: identityDsp with: ascending with: NULL!
		*/
	}

	public void finishCreate(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending, OrderSpec descending) {
		myEmptyRegion = emptyRegion;
		myFullRegion = fullRegion;
		myIdentityDsp = identityDsp;
		myAscending = ascending;
		if (descending == null && (ascending != null)) {
			myDescending = ascending.reversed();
		} else {
			myDescending = descending;
		}
		/*
		udanax-top.st:14413:CoordinateSpace methodsFor: 'protected: create followup'!
		{void} finishCreate: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			with: ascending {OrderSpec default: NULL}
			with: descending {OrderSpec default: NULL}
			
			myEmptyRegion := emptyRegion.
			myFullRegion := fullRegion.
			myIdentityDsp := identityDsp.
			myAscending := ascending.
			(descending == NULL and: [ascending ~~ NULL]) ifTrue:
				[myDescending := ascending reversed]
			ifFalse:
				[myDescending := descending].!
		*/
	}

	protected CoordinateSpace() {
		super();
		myEmptyRegion = null;
		myFullRegion = null;
		myIdentityDsp = null;
		myAscending = null;
		myDescending = null;
		/*
		udanax-top.st:14430:CoordinateSpace methodsFor: 'create'!
		create
			
			super create.
			myEmptyRegion := NULL.
			myFullRegion := NULL.
			myIdentityDsp := NULL.
			myAscending := NULL.
			myDescending := NULL.!
		*/
	}

	protected CoordinateSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending, OrderSpec descending) {
		super();
		myEmptyRegion = emptyRegion;
		myFullRegion = fullRegion;
		myIdentityDsp = identityDsp;
		myAscending = ascending;
		if (descending == null && (ascending != null)) {
			myDescending = ascending.reversed();
		} else {
			myDescending = descending;
		}
		/*
		udanax-top.st:14439:CoordinateSpace methodsFor: 'create'!
		create: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			with: ascending {OrderSpec default: NULL}
			with: descending {OrderSpec default: NULL}
			
			super create.
			myEmptyRegion := emptyRegion.
			myFullRegion := fullRegion.
			myIdentityDsp := identityDsp.
			myAscending := ascending.
			(descending == NULL and: [ascending ~~ NULL]) ifTrue:
				[myDescending := ascending reversed]
			ifFalse:
				[myDescending := descending].!
		*/
	}

	//	public Mapping importMapping(PrimArray data, CoordinateSpace rangeSpace) {
	//		passe();
	//		/*
	//		udanax-top.st:14457:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{Mapping} importMapping: data {PrimArray} with: rangeSpace {CoordinateSpace default: NULL}
	//			self passe!
	//		*/
	//	}

	//	public OrderSpec importOrderSpec(PrimArray data) {
	//		passe();
	//		/*
	//		udanax-top.st:14461:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{OrderSpec} importOrderSpec: data {PrimArray}
	//			self passe!
	//		*/
	//	}

	//	public XnRegion importRegion(PrimArray data) {
	//		passe();
	//		/*
	//		udanax-top.st:14465:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{XnRegion} importRegion: data {PrimArray}
	//			self passe!
	//		*/
	//	}

	//	public Mapping mapping(PrimArray data) {
	//		passe();
	//		/*
	//		udanax-top.st:14469:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{Mapping} mapping: data {PrimArray}
	//			self passe!
	//		*/
	//	}

	//	public Mapping mapping(PrimArray data, CoordinateSpace rangeSpace) {
	//		passe();
	//		/*
	//		udanax-top.st:14473:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{Mapping} mapping: data {PrimArray} with: rangeSpace {CoordinateSpace default: NULL}
	//			self passe!
	//		*/
	//	}

	//	public OrderSpec orderSpec(PrimArray data) {
	//		passe();
	//		/*
	//		udanax-top.st:14477:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{OrderSpec} orderSpec: data {PrimArray}
	//			self passe!
	//		*/
	//	}

	//	public XnRegion region(PrimArray data) {
	//		passe();
	//		/*
	//		udanax-top.st:14481:CoordinateSpace methodsFor: 'smalltalk: passe'!
	//		{XnRegion} region: data {PrimArray}
	//			self passe!
	//		*/
	//	}

//	/**
//	 * {OrderSpec CLIENT} ascending
//	 * {Mapping CLIENT} completeMapping: range {XuRegion}
//	 * {OrderSpec CLIENT} descending
//	 * {XuRegion CLIENT} emptyRegion
//	 * {XuRegion CLIENT} fullRegion
//	 * {Mapping CLIENT} identityMapping
//	 */
//	public static void info() {
//		/*
//		udanax-top.st:14494:CoordinateSpace class methodsFor: 'smalltalk: system'!
//		info.stProtocol
//		"{OrderSpec CLIENT} ascending
//		{Mapping CLIENT} completeMapping: range {XuRegion}
//		{OrderSpec CLIENT} descending
//		{XuRegion CLIENT} emptyRegion
//		{XuRegion CLIENT} fullRegion
//		{Mapping CLIENT} identityMapping
//		"!
//		*/
//	}
}
