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
package org.abora.white.spaces.integers;

import org.abora.white.collection.arrays.IntegerVarArray;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.SpecialistRcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.IntegerUpOrder;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * The space of all integers.  See the class comments in IntegerRegion, XuInteger, and
 * IntegerDsp for interesting properties of this space.  Especially IntegerRegion.
 * IntegerSpaces are the most frequently used of the coordinate spaces.  XuArrays are an
 * efficient data structure which we provide as a table whose domain space is an integer
 * space.  In so doing, the notion of an array is made to be simply a particular case of a
 * table indexed by the positions of a coordinate space.  However, IntegerSpaces and XuArrays
 * are both expected to be more efficient than other spaces and tables built on other spaces.
 * See XuArray
 */
public class IntegerSpace extends CoordinateSpace {
	protected static IntegerSpace TheIntegerSpace = new IntegerSpace();
	/*
	udanax-top.st:15365:
	CoordinateSpace subclass: #IntegerSpace
		instanceVariableNames: ''
		classVariableNames: 'TheIntegerSpace {IntegerSpace} '
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Integers'!
	*/
	/*
	udanax-top.st:15369:
	IntegerSpace comment:
	'The space of all integers.  See the class comments in IntegerRegion, XuInteger, and IntegerDsp for interesting properties of this space.  Especially IntegerRegion.
		
		IntegerSpaces are the most frequently used of the coordinate spaces.  XuArrays are an efficient data structure which we provide as a table whose domain space is an integer space.  In so doing, the notion of an array is made to be simply a particular case of a table indexed by the positions of a coordinate space.  However, IntegerSpaces and XuArrays are both expected to be more efficient than other spaces and tables built on other spaces.  See XuArray'!
	*/
	/*
	udanax-top.st:15373:
	(IntegerSpace getOrMakeCxxClassDescription)
		friends:
	'/- friends for class IntegerSpace -/
	friend class IntegerRegion;
	friend class IntegerDsp;
	';
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
	*/
	/*
	udanax-top.st:15450:
	IntegerSpace class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:15453:
	(IntegerSpace getOrMakeCxxClassDescription)
		friends:
	'/- friends for class IntegerSpace -/
	friend class IntegerRegion;
	friend class IntegerDsp;
	';
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
	*/

	public IntegerSpace() {
		super(
			(IntegerRegion.usingx(false, 0, (IntegerVarArray.make(0)))),
			(IntegerRegion.usingx(true, 0, (IntegerVarArray.make(0)))),
			IntegerMapping.identity(),
			IntegerUpOrder.make());
		/*
		udanax-top.st:15384:IntegerSpace methodsFor: 'creation'!
		create
			super create: (IntegerRegion usingx: false with: Int32Zero
					with: (IntegerVarArray zeros: Int32Zero))
				with: (IntegerRegion usingx: true with: Int32Zero
					with: (IntegerVarArray zeros: Int32Zero))
				with: IntegerMapping identity
				with: IntegerUpOrder make!
		*/
	}

	/**
	 * Essential. Make a region that contains all integers greater than (or equal if inclusive is
	 * true) to start.
	 */
	public IntegerRegion above(IntegerPos start, boolean inclusive) {
		IntegerValue after;
		after = start.asIntegerVar();
		if (!inclusive) {
			after = after.plus(IntegerValue.one());
		}
		return IntegerRegion.after(after);
		/*
		udanax-top.st:15395:IntegerSpace methodsFor: 'making'!
		{IntegerRegion CLIENT} above: start {IntegerPos} with: inclusive {BooleanVar}
			"Essential. Make a region that contains all integers greater than (or equal if inclusive is true) to start."
			| after {IntegerVar} |
			after _ start asIntegerVar.
			inclusive ifFalse: [after _ after + 1].
			^IntegerRegion after: after!
		*/
	}

	/**
	 * Make a region that contains all integers less than (or equal if inclusive is true) to
	 * stop.
	 */
	public IntegerRegion below(IntegerPos stop, boolean inclusive) {
		IntegerValue after;
		after = stop.asIntegerVar();
		if (inclusive) {
			after = after.plus(IntegerValue.one());
		}
		return IntegerRegion.before(after);
		/*
		udanax-top.st:15402:IntegerSpace methodsFor: 'making'!
		{IntegerRegion CLIENT} below: stop {IntegerPos} with: inclusive {BooleanVar}
			"Make a region that contains all integers less than (or equal if inclusive is true) to stop."
			| after {IntegerVar} |
			after _ stop asIntegerVar.
			inclusive ifTrue: [after _ after + 1].
			^IntegerRegion before: after!
		*/
	}

	/**
	 * Make a region that contains all integers greater than or equal to start and less than
	 * stop.
	 */
	public IntegerRegion interval(IntegerPos start, IntegerPos stop) {
		return IntegerRegion.make(start.asIntegerVar(), stop.asIntegerVar());
		/*
		udanax-top.st:15409:IntegerSpace methodsFor: 'making'!
		{IntegerRegion CLIENT} interval: start {IntegerPos} with: stop {IntegerPos}
			"Make a region that contains all integers greater than or equal to start and less than stop."
			
			^IntegerRegion make: start asIntegerVar with: stop asIntegerVar!
		*/
	}

	/**
	 * Essential. Make an integer Position object
	 */
	public IntegerPos position(IntegerValue value) {
		return value.integer();
		/*
		udanax-top.st:15414:IntegerSpace methodsFor: 'making'!
		{IntegerPos CLIENT INLINE} position: value {IntegerVar}
			"Essential. Make an integer Position object"
			
			^value integer!
		*/
	}

	/**
	 * Essential. Make a Mapping which adds a fixed amount to any value.
	 * Should this just be supplanted by CoordinateSpace::mapping ()?
	 */
	public IntegerMapping translation(IntegerValue value) {
		if (value.isEqual(IntegerValue.zero())) {
			return (IntegerMapping) identityDsp();
		}
		return IntegerMapping.make(value);
		/*
		udanax-top.st:15419:IntegerSpace methodsFor: 'making'!
		{IntegerMapping CLIENT} translation: value {IntegerVar}
			"Essential. Make a Mapping which adds a fixed amount to any value.
			Should this just be supplanted by CoordinateSpace::mapping ()?"
			
			value = IntegerVarZero ifTrue:
				[^self identityDsp cast: IntegerMapping].
			^IntegerMapping make: value!
		*/
	}

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
		//		TODOreturn getCategory().hashForEqual() + 1;
		/*
		udanax-top.st:15429:IntegerSpace methodsFor: 'testing'!
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
		udanax-top.st:15434:IntegerSpace methodsFor: 'testing'!
		{BooleanVar} isEqual: anObject {Heaper}
			"is equal to any basic space on the same category of positions"
			^anObject getCategory == self getCategory!
		*/
	}

	//	public IntegerPos integer(IntegerVar value) {
	//		passe()
	//		/* position */;
	//		/*
	//		udanax-top.st:15441:IntegerSpace methodsFor: 'smalltalk: passe'!
	//		{IntegerPos} integer: value {IntegerVar}
	//			self passe "position"!
	//		*/
	//	}

	public void sendSelfTo(Xmtr xmtr) {
		/*
		udanax-top.st:15447:IntegerSpace methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}!
		*/
	}

	/**
	 * Get the receievr for wire requests.
	 */
	public static IntegerSpace implicitReceiver() {
		return TheIntegerSpace;
		/*
		udanax-top.st:15464:IntegerSpace class methodsFor: 'creation'!
		{IntegerSpace INLINE} implicitReceiver
			"Get the receievr for wire requests."
			
			^TheIntegerSpace!
		*/
	}

	/**
	 * return the one integer space
	 */
	public static IntegerSpace make() {
		return TheIntegerSpace;
		/*
		udanax-top.st:15469:IntegerSpace class methodsFor: 'creation'!
		{IntegerSpace CLIENT INLINE} make
			"return the one integer space"
			
			^TheIntegerSpace!
		*/
	}

	public static Heaper make(Rcvr rcvr) {
		((SpecialistRcvr) rcvr).registerIbid(TheIntegerSpace);
		return TheIntegerSpace;
		/*
		udanax-top.st:15476:IntegerSpace class methodsFor: 'rcvr pseudo constructor'!
		{Heaper} make.Rcvr: rcvr {Rcvr}
			(rcvr cast: SpecialistRcvr) registerIbid: TheIntegerSpace.
			^TheIntegerSpace!
		*/
	}

	//	public static void initTimeNonInherited() {
	//		TheIntegerSpace = new IntegerSpace();
	//		/*
	//		udanax-top.st:15482:IntegerSpace class methodsFor: 'smalltalk: init'!
	//		initTimeNonInherited
	//			TheIntegerSpace := self create!
	//		*/
	//	}

	//	public static void linkTimeNonInherited() {
	//		TheIntegerSpace = null;
	//		/*
	//		udanax-top.st:15486:IntegerSpace class methodsFor: 'smalltalk: init'!
	//		linkTimeNonInherited
	//			TheIntegerSpace := NULL!
	//		*/
	//	}

	/**
	 * {IntegerRegion CLIENT} above: start {IntegerVar} with: inclusive {BooleanVar}
	 * {IntegerRegion CLIENT} below: start {IntegerVar} with: inclusive {BooleanVar}
	 * {IntegerRegion CLIENT} interval: start {IntegerVar} with: stop {IntegerVar}
	 * {XuInteger CLIENT} position: value {IntegerVar}
	 * {IntegerMapping CLIENT} translation: value {IntegerVar}
	 */
	public static void info() {
		/*
		udanax-top.st:15492:IntegerSpace class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{IntegerRegion CLIENT} above: start {IntegerVar} with: inclusive {BooleanVar}
		{IntegerRegion CLIENT} below: start {IntegerVar} with: inclusive {BooleanVar}
		{IntegerRegion CLIENT} interval: start {IntegerVar} with: stop {IntegerVar}
		{XuInteger CLIENT} position: value {IntegerVar}
		{IntegerMapping CLIENT} translation: value {IntegerVar}
		"!
		*/
	}
}
