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

import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.sets.ScruSet;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.OrderSpec;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * A cross region is a distinction if 1) it is empty, 2) it is full, or 3) it is the
 * rectangular cross of full regions and one distinction. Note that case 3 actually subsumes
 * 1 and 2.  Since the simple regions of a space are the intersections of a finite number of
 * distinctions of a space, this implies that A cross region is simple if it is the
 * rectangular cross of simple regions.  In other words, a simple region is identical to the
 * cross of its projections.
 */
public abstract class CrossRegion extends XnRegion {
	/*
	udanax-top.st:65511:
	XnRegion subclass: #CrossRegion
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Cross'!
	*/
	/*
	udanax-top.st:65515:
	CrossRegion comment:
	'A cross region is a distinction if 1) it is empty, 2) it is full, or 3) it is the rectangular cross of full regions and one distinction. Note that case 3 actually subsumes 1 and 2.  Since the simple regions of a space are the intersections of a finite number of distinctions of a space, this implies that A cross region is simple if it is the rectangular cross of simple regions.  In other words, a simple region is identical to the cross of its projections.'!
	*/
	/*
	udanax-top.st:65517:
	(CrossRegion getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:65620:
	CrossRegion class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:65623:
	(CrossRegion getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected CrossRegion() {
		super();
	}
	
	protected CrossRegion(Rcvr rcvr) {
		super(rcvr);
	}

	/**
	 * To avoid overly burdensome canonicalization rules, my hash is calculated from the hash of
	 * my projections
	 */
	public int actualHashForEqual() {
		return getClass().hashCode() ^ projections().contentsHash();
		/*
		udanax-top.st:65522:CrossRegion methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			"To avoid overly burdensome canonicalization rules, my hash is calculated from the hash of my projections"
			
			^#cat.U.CrossRegion hashForEqual bitXor: self projections contentsHash.!
		*/
	}

	public abstract boolean hasMember(Position atPos);
	/*
	udanax-top.st:65527:CrossRegion methodsFor: 'testing'!
	{BooleanVar} hasMember: atPos {Position unused} 
		self subclassResponsibility!
	*/

	public abstract boolean isEmpty();
	/*
	udanax-top.st:65531:CrossRegion methodsFor: 'testing'!
	{BooleanVar} isEmpty
		self subclassResponsibility!
	*/

	public abstract boolean isEnumerable(OrderSpec order);
	/*
	udanax-top.st:65535:CrossRegion methodsFor: 'testing'!
	{BooleanVar} isEnumerable: order {OrderSpec unused default: NULL}
		
		self subclassResponsibility!
	*/

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:65539:CrossRegion methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		
		self subclassResponsibility!
	*/

	public abstract boolean isFinite();
	/*
	udanax-top.st:65543:CrossRegion methodsFor: 'testing'!
	{BooleanVar} isFinite
		self subclassResponsibility!
	*/

	public abstract boolean isSimple();
	/*
	udanax-top.st:65547:CrossRegion methodsFor: 'testing'!
	{BooleanVar} isSimple
		
		self subclassResponsibility!
	*/

	/**
	 * Essential. Divide this Region up into a disjoint sequence of boxes. A box is a region
	 * which is the cross of its projections.
	 */
	public abstract Stepper boxes();
	/*
	udanax-top.st:65553:CrossRegion methodsFor: 'enumerating'!
	{Stepper CLIENT of: CrossRegion} boxes
		"Essential. Divide this Region up into a disjoint sequence of boxes. A box is a region which is the cross of its projections."
		
		self subclassResponsibility!
	*/

	public abstract IntegerValue count();
	/*
	udanax-top.st:65558:CrossRegion methodsFor: 'enumerating'!
	{IntegerVar} count
		
		self subclassResponsibility!
	*/

	public abstract ScruSet distinctions();
	/*
	udanax-top.st:65562:CrossRegion methodsFor: 'enumerating'!
	{ScruSet of: XnRegion} distinctions
		self subclassResponsibility!
	*/

	/**
	 * Whether this Region is a box, i.e. is equal to the cross of its projections.
	 */
	public abstract boolean isBox();
	/*
	udanax-top.st:65566:CrossRegion methodsFor: 'enumerating'!
	{BooleanVar CLIENT} isBox
		"Whether this Region is a box, i.e. is equal to the cross of its projections."
		
		self subclassResponsibility!
	*/

	public abstract Stepper simpleRegions(OrderSpec order);
	/*
	udanax-top.st:65571:CrossRegion methodsFor: 'enumerating'!
	{Stepper} simpleRegions: order {OrderSpec default: NULL} 
		self subclassResponsibility!
	*/

	public abstract XnRegion asSimpleRegion();
	/*
	udanax-top.st:65577:CrossRegion methodsFor: 'operations'!
	{XnRegion} asSimpleRegion
		self subclassResponsibility!
	*/

	public abstract XnRegion complement();
	/*
	udanax-top.st:65581:CrossRegion methodsFor: 'operations'!
	{XnRegion} complement
		self subclassResponsibility!
	*/

	public abstract XnRegion intersect(XnRegion other);
	/*
	udanax-top.st:65585:CrossRegion methodsFor: 'operations'!
	{XnRegion} intersect: other {XnRegion unused} 
		self subclassResponsibility!
	*/

	public XnRegion simpleUnion(XnRegion other) {
		return (unionWith(other)).asSimpleRegion();
		/*
		udanax-top.st:65589:CrossRegion methodsFor: 'operations'!
		{XnRegion} simpleUnion: other {XnRegion} 
			
			^(self unionWith: other) asSimpleRegion!
		*/
	}

	public abstract XnRegion unionWith(XnRegion other);
	/*
	udanax-top.st:65593:CrossRegion methodsFor: 'operations'!
	{XnRegion} unionWith: other {XnRegion unused} 
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:65599:CrossRegion methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		
		self subclassResponsibility!
	*/

	/**
	 * The answer is the projection of this region into the specified dimension of the cross
	 * space
	 */
	public XnRegion projection(int index) {
		return (XnRegion) (projections().fetch(index));
		/*
		udanax-top.st:65603:CrossRegion methodsFor: 'accessing'!
		{XnRegion CLIENT} projection: index {Int32}
			"The answer is the projection of this region into the specified dimension of the cross space"
			
			^(self projections fetch: index) cast: XnRegion!
		*/
	}

	/**
	 * Essential.  The answer is the projection of this region into each dimension of the cross
	 * space. Note that two regions which are different can have the same projections.
	 */
	public abstract PtrArray projections();
	/*
	udanax-top.st:65608:CrossRegion methodsFor: 'accessing'!
	{PtrArray CLIENT of: XnRegion} projections
		"Essential.  The answer is the projection of this region into each dimension of the cross space. Note that two regions which are different can have the same projections."
		
		self subclassResponsibility!
	*/

	public abstract Stepper actualStepper(OrderSpec order);
	/*
	udanax-top.st:65615:CrossRegion methodsFor: 'protected: enumerating'!
	{Stepper of: Position} actualStepper: order {OrderSpec} 
		self subclassResponsibility!
	*/

	/**
	 * {Stepper CLIENT of: CrossRegion} boxes
	 * {BooleanVar CLIENT} isBox
	 * {XuRegion CLIENT} projection: index {Int32}
	 * {PtrArray CLIENT of: XuRegion} projections
	 */
	public static void info() {
		/*
		udanax-top.st:65628:CrossRegion class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{Stepper CLIENT of: CrossRegion} boxes
		{BooleanVar CLIENT} isBox
		{XuRegion CLIENT} projection: index {Int32}
		{PtrArray CLIENT of: XuRegion} projections
		"!
		*/
	}
}
