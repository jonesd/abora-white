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
package org.abora.white.spaces.unordered;

import org.abora.white.hspace.HeaperRegion;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.SpecialistRcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.xpp.basic.Heaper;

/**
 * A HeaperSpace is one whose positions represent the identity of individual Heapers.
 * Identity of a Heaper is determined according by its response to "isEqual" and
 * "hashForEqual" (see "The Equality of Decisions" for a bunch of surprising issues regarding
 * Heaper equality).  A region is a HeaperSpace is a SetRegion (see SetRegion).  As a result
 * of having HeaperSpaces, one can use the identity of Heapers to index into hash tables, and
 * still obey the convention that a table maps from positions in some coordinate space.
 * HeaperSpaces cannot (yet?) be used as the domain space for Xanadu Stamps, and therefore
 * also not as the domain space of an IndexedWaldo.  In order to do this, the Heapers in
 * question would have to persist in a way that Xanadu doesn''t provide for.
 * As is typical for an unordered space, the only Dsp for this space is the identity Dsp.  No
 * type or pseudo-constructor is exported however--the Dsp is gotten by converting a
 * HeaperSpace to a Dsp.  Similarly, no heaper-specific type or pseudo-constructor is
 * exported for my regions.  The conversions are sufficient.  The resulting regions are
 * guaranteed to be SetRegions.
 */
public class HeaperSpace extends CoordinateSpace {
	protected static HeaperSpace TheHeaperSpace = new HeaperSpace();
	/*
	udanax-top.st:15012:
	CoordinateSpace subclass: #HeaperSpace
		instanceVariableNames: ''
		classVariableNames: 'TheHeaperSpace {HeaperSpace} '
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Unordered'!
	*/
	/*
	udanax-top.st:15016:
	HeaperSpace comment:
	'A HeaperSpace is one whose positions represent the identity of individual Heapers.  Identity of a Heaper is determined according by its response to "isEqual" and "hashForEqual" (see "The Equality of Decisions" for a bunch of surprising issues regarding Heaper equality).  A region is a HeaperSpace is a SetRegion (see SetRegion).  As a result of having HeaperSpaces, one can use the identity of Heapers to index into hash tables, and still obey the convention that a table maps from positions in some coordinate space.
		
		HeaperSpaces cannot (yet?) be used as the domain space for Xanadu Stamps, and therefore also not as the domain space of an IndexedWaldo.  In order to do this, the Heapers in question would have to persist in a way that Xanadu doesn''t provide for.
		
		As is typical for an unordered space, the only Dsp for this space is the identity Dsp.  No type or pseudo-constructor is exported however--the Dsp is gotten by converting a HeaperSpace to a Dsp.  Similarly, no heaper-specific type or pseudo-constructor is exported for my regions.  The conversions are sufficient.  The resulting regions are guaranteed to be SetRegions.'!
	*/
	/*
	udanax-top.st:15022:
	(HeaperSpace getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #PSEUDO.COPY; yourself)!
	*/
	/*
	udanax-top.st:15052:
	HeaperSpace class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:15055:
	(HeaperSpace getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #PSEUDO.COPY; yourself)!
	*/

	public HeaperSpace() {
		super(HeaperRegion.make(), HeaperRegion.make().complement(), HeaperDsp.make(), null, null);
		/*
		udanax-top.st:15027:HeaperSpace methodsFor: 'creation'!
		create
			super create: HeaperRegion make
				with: HeaperRegion make complement
				with: HeaperDsp make
				with: NULL
				with: NULL!
		*/
	}

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
		//		TODOreturn getCategory().hashForEqual() + 1;
		/*
		udanax-top.st:15037:HeaperSpace methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			"is equal to any basic space on the same category of positions"
			^self getCategory hashForEqual + 1!
		*/
	}

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public boolean isEqual(Heaper anObject) {
		return anObject.getClass() == getClass();
		/*
		udanax-top.st:15042:HeaperSpace methodsFor: 'testing'!
		{BooleanVar} isEqual: anObject {Heaper}
			"is equal to any basic space on the same category of positions"
			^anObject getCategory == self getCategory!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		/*
		udanax-top.st:15049:HeaperSpace methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}!
		*/
	}

	//	public static void initTimeNonInherited() {
	//		TheHeaperSpace = new HeaperSpace();
	//		/*
	//		udanax-top.st:15060:HeaperSpace class methodsFor: 'smalltalk: init'!
	//		initTimeNonInherited
	//			TheHeaperSpace := self create!
	//		*/
	//	}

	//	public static void linkTimeNonInherited() {
	//		TheHeaperSpace = null;
	//		/*
	//		udanax-top.st:15064:HeaperSpace class methodsFor: 'smalltalk: init'!
	//		linkTimeNonInherited
	//			TheHeaperSpace := NULL!
	//		*/
	//	}

	/**
	 * Return the one instance of HeaperSpace
	 */
	public static HeaperSpace make() {
		return TheHeaperSpace;
		/*
		udanax-top.st:15070:HeaperSpace class methodsFor: 'pseudo constructors'!
		{HeaperSpace INLINE} make
			"Return the one instance of HeaperSpace"
			
			^TheHeaperSpace!
		*/
	}

	public static Heaper make(Rcvr rcvr) {
		((SpecialistRcvr) rcvr).registerIbid(TheHeaperSpace);
		return TheHeaperSpace;
		/*
		udanax-top.st:15077:HeaperSpace class methodsFor: 'rcvr pseudo constructor'!
		{Heaper} make.Rcvr: rcvr {Rcvr}
			(rcvr cast: SpecialistRcvr) registerIbid: TheHeaperSpace.
			^TheHeaperSpace!
		*/
	}
}
