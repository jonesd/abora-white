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
package org.abora.white.cross;

import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.Dsp;
import org.abora.white.spaces.basic.Mapping;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.spaces.cross.CrossSpace;

/**
 * All other crossed mappings must be gotten by factoring the non-dsp aspects out into the
 * generic non-dsp mapping objects.  This class represents what remains after the factoring.
 */
public abstract class CrossMapping extends Dsp {
	/*
	udanax-top.st:29169:
	Dsp subclass: #CrossMapping
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-cross'!
	*/
	/*
	udanax-top.st:29173:
	CrossMapping comment:
	'All other crossed mappings must be gotten by factoring the non-dsp aspects out into the generic non-dsp mapping objects.  This class represents what remains after the factoring.'!
	*/
	/*
	udanax-top.st:29175:
	(CrossMapping getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:29229:
	CrossMapping class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:29232:
	(CrossMapping getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected CrossMapping() {
		super();
	}

	protected CrossMapping(Rcvr rcvr) {
		super(rcvr);
	}

	public abstract XnRegion ofAll(XnRegion reg);
	/*
	udanax-top.st:29180:CrossMapping methodsFor: 'transforming'!
	{XnRegion} ofAll: reg {XnRegion}
		
		self subclassResponsibility!
	*/

	public abstract Dsp compose(Dsp other);
	/*
	udanax-top.st:29186:CrossMapping methodsFor: 'combining'!
	{Dsp} compose: other {Dsp}
		
		self subclassResponsibility!
	*/

	public abstract Mapping inverse();
	/*
	udanax-top.st:29190:CrossMapping methodsFor: 'combining'!
	{Mapping} inverse
		self subclassResponsibility!
	*/

	public abstract Dsp minus(Dsp other);
	/*
	udanax-top.st:29194:CrossMapping methodsFor: 'combining'!
	{Dsp} minus: other {Dsp}
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:29200:CrossMapping methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		
		self subclassResponsibility!
	*/

	public abstract boolean isIdentity();
	/*
	udanax-top.st:29204:CrossMapping methodsFor: 'accessing'!
	{BooleanVar} isIdentity
		
		self subclassResponsibility!
	*/

	/**
	 * The Dsp applied to Positions in the given subspace.
	 */
	public abstract Dsp subMapping(int index);
	/*
	udanax-top.st:29208:CrossMapping methodsFor: 'accessing'!
	{Dsp CLIENT} subMapping: index {Int32}
		"The Dsp applied to Positions in the given subspace."
		
		self subclassResponsibility!
	*/

	/**
	 * The Mappings applied to Positions in each of the subspaces. Each of these is already
	 * simple enough that it is either the identityMapping or a visible subclass like
	 * IntegerMapping.
	 */
	public abstract PtrArray subMappings();
	/*
	udanax-top.st:29213:CrossMapping methodsFor: 'accessing'!
	{PtrArray CLIENT of: Dsp} subMappings
		"The Mappings applied to Positions in each of the subspaces. Each of these is already simple enough that it is either the identityMapping or a visible subclass like IntegerMapping."
		
		self subclassResponsibility!
	*/

	//	public Dsp subDsp(int index) {
	//		passe();
	//		/*
	//		udanax-top.st:29220:CrossMapping methodsFor: 'smalltalk: passe'!
	//		{Dsp} subDsp: index {Int32}
	//			self passe!
	//		*/
	//	}

	//	public PtrArray subDsps() {
	//		passe()
	//		/* subMappings */;
	//		/*
	//		udanax-top.st:29224:CrossMapping methodsFor: 'smalltalk: passe'!
	//		{PtrArray of: Dsp} subDsps
	//			self passe "subMappings"!
	//		*/
	//	}

	public static CrossMapping make(CrossSpace space, PtrArray subDsps) {
		PtrArray subDs = PtrArray.make(space.axisCount());
		for (int i = 0; i < subDs.count(); i++) {
			subDs.store(i, (space.axis(i)).identityDsp());
		}
		if (subDsps != null) {
			for (int i = 0; i < subDs.count(); i++) {
				Dsp subDsp;
				if ((subDsp = (Dsp) (subDsps.fetch(i))) != null) {
					subDs.store(i, subDsp);
				}
			}
		}
		return new GenericCrossDsp(space, subDs);
		/*
		udanax-top.st:29237:CrossMapping class methodsFor: 'pseudoconstructors'!
		make: space {CrossSpace} with: subDsps {(PtrArray of: Dsp | NULL) default: NULL}
			| subDs {PtrArray of: Dsp} |
			subDs := PtrArray nulls: space axisCount.
			Int32Zero almostTo: subDs count do: [:i {Int32} |
				subDs at: i store: (space axis: i) identityDsp].
			
			subDsps ~~ NULL ifTrue:
				[Int32Zero almostTo: subDs count do: [:i {Int32} |
					| subDsp {Dsp | NULL} |
					(subDsp := (subDsps fetch: i) cast: Dsp) ~~ NULL ifTrue:
						[subDs at: i store: subDsp]]].
			
			^GenericCrossDsp create: space with: subDs!
		*/
	}


	public static CrossMapping make(CrossSpace space) {
		return CrossMapping.make(space, (PtrArray)null);
	}

	//	public static Heaper make(Object space) {
	//		return make(space, null);
	//		/*
	//		udanax-top.st:29254:CrossMapping class methodsFor: 'smalltalk: defaults'!
	//		make: space
	//			^self make: space with: NULL!
	//		*/
	//	}

	/**
	 * {Dsp CLIENT} subMapping: index {Int32}
	 * {PtrArray CLIENT of: Dsp} subMappings
	 */
	public static void info() {
		/*
		udanax-top.st:29260:CrossMapping class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{Dsp CLIENT} subMapping: index {Int32}
		{PtrArray CLIENT of: Dsp} subMappings
		"!
		*/
	}
}
