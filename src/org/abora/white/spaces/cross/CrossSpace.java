/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * $Id$
 */
package org.abora.white.spaces.cross;

import org.abora.white.collection.arrays.PrimIntArray;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.cross.CrossOrderSpec;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.Mapping;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.PrimSpec;
import org.abora.white.xpp.basic.Heaper;

/**
 * Represents the cross of several coordinate spaces.
 */
public abstract class CrossSpace extends CoordinateSpace {
	protected PtrArray mySubSpaces;
	/*
	udanax-top.st:14573:
	CoordinateSpace subclass: #CrossSpace
		instanceVariableNames: 'mySubSpaces {PtrArray of: CoordinateSpace}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Cross'!
	*/
	/*
	udanax-top.st:14577:
	CrossSpace comment:
	'Represents the cross of several coordinate spaces.  '!
	*/
	/*
	udanax-top.st:14579:
	(CrossSpace getOrMakeCxxClassDescription)
		friends:
	'friend class BoxAccumulator;
	friend class BoxStepper;
	friend class GenericCrossSpace;
	friend class GenericCrossRegion;
	friend class BoxProjectionStepper;';
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:14724:
	CrossSpace class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:14727:
	(CrossSpace getOrMakeCxxClassDescription)
		friends:
	'friend class BoxAccumulator;
	friend class BoxStepper;
	friend class GenericCrossSpace;
	friend class GenericCrossRegion;
	friend class BoxProjectionStepper;';
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/

	/**
	 * Essential.  The base spaces that I am a cross of.
	 */
	public PtrArray axes() {
		return (PtrArray) mySubSpaces.copy();
		/*
		udanax-top.st:14590:CrossSpace methodsFor: 'accessing'!
		{PtrArray CLIENT of: CoordinateSpace} axes
			"Essential.  The base spaces that I am a cross of."
			^mySubSpaces copy cast: PtrArray!
		*/
	}

	/**
	 * The sub coordinate space on the given axis
	 */
	public CoordinateSpace axis(int dimension) {
		return (CoordinateSpace) (mySubSpaces.fetch(dimension));
		/*
		udanax-top.st:14595:CrossSpace methodsFor: 'accessing'!
		{CoordinateSpace CLIENT} axis: dimension {Int32}
			"The sub coordinate space on the given axis"
			
			^(mySubSpaces fetch: dimension) cast: CoordinateSpace!
		*/
	}

	/**
	 * The number of dimensions in this space
	 */
	public int axisCount() {
		return mySubSpaces.count();
		/*
		udanax-top.st:14600:CrossSpace methodsFor: 'accessing'!
		{Int32 CLIENT INLINE} axisCount
			"The number of dimensions in this space"
			
			^mySubSpaces count!
		*/
	}

	public int actualHashForEqual() {
		return mySubSpaces.contentsHash() ^ getClass().hashCode();
		/*
		udanax-top.st:14607:CrossSpace methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^mySubSpaces contentsHash bitXor: #cat.U.CrossSpace hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof CrossSpace) {
			CrossSpace cross = (CrossSpace) other;
			return cross.secretSubSpaces().contentsEqual(mySubSpaces);
		} else {
			return false;
		}
		/*
		udanax-top.st:14611:CrossSpace methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			
			other 
				cast: CrossSpace into: [:cross |
					^cross secretSubSpaces contentsEqual: mySubSpaces]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	/**
	 * Essential.  Map each coordinate according to the mapping from its space.  NULLs mean 'use
	 * the identity mapping'
	 */
	public abstract Mapping crossOfMappings(PtrArray subMappings);
	/*
	udanax-top.st:14621:CrossSpace methodsFor: 'making'!
	{Mapping CLIENT} crossOfMappings: subMappings {(PtrArray of: Mapping | NULL) default: NULL}
		"Essential.  Map each coordinate according to the mapping from its space.  NULLs mean 'use the identity mapping'"
		
		self subclassResponsibility!
	*/

	/**
	 * Essential.  Make a lexical ordering of all elements in the space, using the given ordering
	 * for each sub space. If no sub space ordering is given, then it is in the order they are in
	 * the array.
	 * subSpaceOrdering lists the lexicographic order in which each dimension should be
	 * processed.  Every dimension should be listed exactly one, from most significant (at index
	 * 0) to least significant.
	 * subOrderings are indexed by *dimension*, not by lexicographic order.  In order to index by
	 * lex order, look up the dimension in subSpaceOrdering, and then look up the resulting
	 * dimension number in subOrderings.
	 */
	public abstract CrossOrderSpec crossOfOrderSpecs(PtrArray subOrderings, PrimIntArray subSpaceOrdering);
	/*
	udanax-top.st:14626:CrossSpace methodsFor: 'making'!
	{CrossOrderSpec CLIENT} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL) default: NULL}
		with: subSpaceOrdering {PrimIntArray default: NULL}
		"Essential.  Make a lexical ordering of all elements in the space, using the given ordering for each sub space. If no sub space ordering is given, then it is in the order they are in the array.
		
		subSpaceOrdering lists the lexicographic order in which each dimension should be processed.  Every dimension should be listed exactly one, from most significant (at index 0) to least significant.
		subOrderings are indexed by *dimension*, not by lexicographic order.  In order to index by lex order, look up the dimension in subSpaceOrdering, and then look up the resulting dimension number in subOrderings."
		
		self subclassResponsibility!
	*/

	/**
	 * Essential. Make an individual position
	 */
	public abstract Tuple crossOfPositions(PtrArray coordinates);
	/*
	udanax-top.st:14636:CrossSpace methodsFor: 'making'!
	{Tuple CLIENT} crossOfPositions: coordinates {PtrArray of: Position}
		"Essential. Make an individual position"
		
		self subclassResponsibility!
	*/

	/**
	 * Essential. Make a 'rectangular' region as a cross of all the given regions
	 */
	public abstract CrossRegion crossOfRegions(PtrArray subRegions);
	/*
	udanax-top.st:14641:CrossSpace methodsFor: 'making'!
	{CrossRegion CLIENT} crossOfRegions: subRegions {PtrArray of: XnRegion | NULL}
		"Essential. Make a 'rectangular' region as a cross of all the given regions"
		
		self subclassResponsibility!
	*/

	/**
	 * Return a region whose projection is 'subRegion' along 'dimension', but is full on all
	 * other dimensions
	 */
	public abstract CrossRegion extrusion(int dimension, XnRegion subRegion);
	/*
	udanax-top.st:14646:CrossSpace methodsFor: 'making'!
	{CrossRegion CLIENT} extrusion: dimension {Int32} with: subRegion {XnRegion}
		"Return a region whose projection is 'subRegion' along 'dimension', but is full on all other dimensions"
		
		self subclassResponsibility!
	*/

	//	public IntegerVar count() {
	//		passe()
	//		/* axisCount */;
	//		/*
	//		udanax-top.st:14653:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{IntegerVar} count
	//			self passe "axisCount"!
	//		*/
	//	}

	//	public int intCount() {
	//		passe()
	//		/* axisCount */;
	//		/*
	//		udanax-top.st:14657:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{Int32} intCount
	//			self passe "axisCount"!
	//		*/
	//	}

	//	public CrossMapping makeCrossMapping(PtrArray subMappings) {
	//		passe();
	//		/*
	//		udanax-top.st:14661:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{CrossMapping} makeCrossMapping: subMappings {PtrArray of: Mapping}
	//			
	//			self passe!
	//		*/
	//	}

	//	/**
	//	 * Make a lexical ordering of all elements in the space, using the given ordering for each
	//	 * sub space. If no sub space ordering is given, then it is in the order they are in the
	//	 * array
	//	 */
	//	public CrossOrderSpec makeCrossOrderSpec(PtrArray subOrderings, Int32Array subSpaceOrdering) {
	//		passe();
	//		/*
	//		udanax-top.st:14665:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{CrossOrderSpec} makeCrossOrderSpec: subOrderings {PtrArray of: OrderSpec | NULL}
	//			with: subSpaceOrdering {Int32Array default: NULL}
	//			"Make a lexical ordering of all elements in the space, using the given ordering for each sub space. If no sub space ordering is given, then it is in the order they are in the array"
	//			
	//			self passe!
	//		*/
	//	}

	//	/**
	//	 * Make a 'rectangular' region as a cross of all the given regions
	//	 */
	//	public CrossRegion makeCrossRegion(PtrArray subRegions) {
	//		passe();
	//		/*
	//		udanax-top.st:14671:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{CrossRegion} makeCrossRegion: subRegions {PtrArray of: XnRegion | NULL}
	//			"Make a 'rectangular' region as a cross of all the given regions"
	//			
	//			self passe!
	//		*/
	//	}

	//	/**
	//	 * Make an individual position
	//	 */
	//	public Tuple makeTuple(PtrArray coordinates) {
	//		passe();
	//		/*
	//		udanax-top.st:14676:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{Tuple} makeTuple: coordinates {PtrArray of: Position}
	//			"Make an individual position"
	//			
	//			self passe!
	//		*/
	//	}

	//	public CoordinateSpace subSpace(int dimension) {
	//		passe()
	//		/* axis */;
	//		/*
	//		udanax-top.st:14681:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{CoordinateSpace} subSpace: dimension {Int32}
	//			self passe "axis"!
	//		*/
	//	}

	//	public PtrArray subSpaces() {
	//		passe()
	//		/* axes */;
	//		/*
	//		udanax-top.st:14685:CrossSpace methodsFor: 'smalltalk: passe'!
	//		{PtrArray of: CoordinateSpace} subSpaces
	//			self passe "axes"!
	//		*/
	//	}

	public Mapping crossOfMappings() {
		return crossOfMappings(null);
		/*
		udanax-top.st:14691:CrossSpace methodsFor: 'smalltalk: defaults'!
		{Mapping CLIENT} crossOfMappings
			^self crossOfMappings: NULL!
		*/
	}

	public CrossOrderSpec crossOfOrderSpecs() {
		return crossOfOrderSpecs(null, null);
		/*
		udanax-top.st:14695:CrossSpace methodsFor: 'smalltalk: defaults'!
		{CrossOrderSpec CLIENT} crossOfOrderSpecs
			^self crossOfOrderSpecs: NULL with: NULL!
		*/
	}

	public CrossOrderSpec crossOfOrderSpecs(PtrArray subOrderings) {
		return crossOfOrderSpecs(subOrderings, null);
		/*
		udanax-top.st:14699:CrossSpace methodsFor: 'smalltalk: defaults'!
		{CrossOrderSpec CLIENT} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL) default: NULL}
			^self crossOfOrderSpecs: subOrderings with: NULL!
		*/
	}

	/**
	 * The actual array of sub spaces. DO NOT MODIFY
	 */
	public PtrArray secretSubSpaces() {
		return mySubSpaces;
		/*
		udanax-top.st:14706:CrossSpace methodsFor: 'protected: accessing'!
		{PtrArray INLINE of: CoordinateSpace} secretSubSpaces
			"The actual array of sub spaces. DO NOT MODIFY"
			
			^mySubSpaces!
		*/
	}

	public CrossSpace() {
		super();
		mySubSpaces = null;
		/*
		udanax-top.st:14713:CrossSpace methodsFor: 'protected: creation'!
		create
			
			super create.
			mySubSpaces := NULL.!
		*/
	}

	public CrossSpace(PtrArray subSpaces) {
		super();
		mySubSpaces = subSpaces;
		/*
		udanax-top.st:14718:CrossSpace methodsFor: 'protected: creation'!
		create: subSpaces {PtrArray of: CoordinateSpace}
			
			super create.
			mySubSpaces := subSpaces.!
		*/
	}

	/**
	 * Make a cross space with the given list of subspaces
	 */
	public static CrossSpace make(PtrArray subSpaces) {
		/* Should use middlemen.  Just hard code special cases for now */
		return GenericCrossSpace.make(((PtrArray) subSpaces.copy()));
		/*
		udanax-top.st:14738:CrossSpace class methodsFor: 'creation'!
		{CrossSpace CLIENT} make: subSpaces {PtrArray of: CoordinateSpace}
			"Make a cross space with the given list of subspaces"
			 "Should use middlemen.  Just hard code special cases for now"
			 
			^GenericCrossSpace make: (subSpaces copy cast: PtrArray)!
		*/
	}

	/**
	 * Cross two sub spaces
	 */
	public static CrossSpace make(CoordinateSpace zeroSpace, CoordinateSpace oneSpace) {
		return new GenericCrossSpace(((PtrArray) (PrimSpec.pointer().arrayWithTwo(zeroSpace, oneSpace))));
		/*
		udanax-top.st:14744:CrossSpace class methodsFor: 'creation'!
		make: zeroSpace {CoordinateSpace} with: oneSpace {CoordinateSpace}
			"Cross two sub spaces"
			
			^GenericCrossSpace create: ((PrimSpec pointer
				arrayWithTwo: zeroSpace with: oneSpace) cast: PtrArray)!
		*/
	}

	/**
	 * {PtrArray CLIENT of: CoordinateSpace} axes
	 * {CoordinateSpace CLIENT} axis: dimension {Int32}
	 * {Int32 CLIENT} axisCount
	 * {Mapping CLIENT} crossOfMappings
	 * {Mapping CLIENT} crossOfMappings: subMappings {(PtrArray of: Mapping | NULL)
	 * default: NULL}
	 * {CrossOrderSpec CLIENT} crossOfOrderSpecs
	 * {CrossOrderSpec CLIENT} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL)
	 * default: NULL}
	 * {CrossOrderSpec CLIENT} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL)
	 * default: NULL} with: subSpaceOrdering {Int32Array default: NULL}
	 * {Tuple CLIENT} crossOfPositions: coordinates {PtrArray of: Position}
	 * {CrossRegion CLIENT} crossOfRegions: subRegions {PtrArray of: XuRegion | NULL}
	 * {CrossRegion CLIENT} extrusion: dimension {Int32} with: subRegion {XuRegion}
	 */
	public static void info() {
		/*
		udanax-top.st:14752:CrossSpace class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{PtrArray CLIENT of: CoordinateSpace} axes
		{CoordinateSpace CLIENT} axis: dimension {Int32}
		{Int32 CLIENT} axisCount
		{Mapping CLIENT} crossOfMappings
		{Mapping CLIENT} crossOfMappings: subMappings {(PtrArray of: Mapping | NULL)
				default: NULL}
		{CrossOrderSpec CLIENT} crossOfOrderSpecs
		{CrossOrderSpec CLIENT} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL)
				default: NULL}
		{CrossOrderSpec CLIENT} crossOfOrderSpecs: subOrderings {(PtrArray of: OrderSpec | NULL)
				default: NULL} with: subSpaceOrdering {Int32Array default: NULL}
		{Tuple CLIENT} crossOfPositions: coordinates {PtrArray of: Position}
		{CrossRegion CLIENT} crossOfRegions: subRegions {PtrArray of: XuRegion | NULL}
		{CrossRegion CLIENT} extrusion: dimension {Int32} with: subRegion {XuRegion}
		"!
		*/
	}
}
