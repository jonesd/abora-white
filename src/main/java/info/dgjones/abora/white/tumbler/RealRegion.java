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
package info.dgjones.abora.white.tumbler;

import java.io.PrintWriter;

import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.collection.arrays.PrimFloatArray;
import info.dgjones.abora.white.collection.arrays.PrimIntegerArray;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.sets.ScruSet;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class RealRegion extends XnRegion {
	protected boolean myStartsInside;
	protected PrimFloatArray myTransitionVals;
	protected PrimIntegerArray myTransitionFlags;
	protected static RealManager TheManager = new RealManager();
	/*
	udanax-top.st:69266:
	XnRegion subclass: #RealRegion
		instanceVariableNames: '
			myStartsInside {BooleanVar}
			myTransitionVals {PrimFloatArray}
			myTransitionFlags {PrimIntegerArray}'
		classVariableNames: 'TheManager {RealManager} '
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:69273:
	(RealRegion getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:69463:
	RealRegion class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:69466:
	(RealRegion getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
	*/

	public IntegerValue count() {
		return TheManager.count(this);
		/*
		udanax-top.st:69278:RealRegion methodsFor: 'enumerating'!
		{IntegerVar INLINE} count
			^TheManager count: self!
		*/
	}

	public ScruSet distinctions() {
		return TheManager.distinctions(this);
		/*
		udanax-top.st:69282:RealRegion methodsFor: 'enumerating'!
		{ScruSet INLINE of: XnRegion} distinctions
			^TheManager distinctions: self!
		*/
	}

	/**
	 * Essential. Break this up into disjoint intervals
	 */
	public Stepper intervals(OrderSpec order) {
		return simpleRegions(order);
		/*
		udanax-top.st:69286:RealRegion methodsFor: 'enumerating'!
		{Stepper CLIENT INLINE of: RealRegion} intervals: order {OrderSpec default: NULL}
			"Essential. Break this up into disjoint intervals"
			
			^ self simpleRegions: order!
		*/
	}

	/**
	 * Whether this Region is a non-empty interval, i.e. if A, B in the Region and A <= C <= B
	 * then C is in the Region. This includes inequalities (e.g. {x | x > 5}) and the fullRegion
	 * in addition to ordinary two-ended intervals.
	 */
	public boolean isInterval() {
		return isSimple();
		/*
		udanax-top.st:69291:RealRegion methodsFor: 'enumerating'!
		{BooleanVar CLIENT INLINE} isInterval
			"Whether this Region is a non-empty interval, i.e. if A, B in the Region and A <= C <= B then C is in the Region. This includes inequalities (e.g. {x | x > 5}) and the fullRegion in addition to ordinary two-ended intervals."
			
			^self isSimple!
		*/
	}

	public Stepper simpleRegions(OrderSpec order) {
		return TheManager.simpleRegions(this, order);
		/*
		udanax-top.st:69296:RealRegion methodsFor: 'enumerating'!
		{Stepper} simpleRegions: order {OrderSpec default: NULL} 
			^TheManager simpleRegions: self with: order!
		*/
	}

	public Stepper stepper(OrderSpec order) {
		if (!isFinite()) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_ENUMERABLE);
		}
		return new RealStepper(secretTransitions());
		/*
		udanax-top.st:69300:RealRegion methodsFor: 'enumerating'!
		{Stepper of: Position} stepper: order {OrderSpec default: NULL} 
			self isFinite ifFalse:
				[Heaper BLAST: #NotEnumerable].
			^RealStepper create: self secretTransitions!
		*/
	}

	public Stepper actualStepper(OrderSpec order) {
		throw new UnsupportedOperationException();
		//		shouldNotImplement();
		//		return null
		//		/* fodder */;
		/*
		udanax-top.st:69308:RealRegion methodsFor: 'protected: enumerating'!
		{Stepper of: Position} actualStepper: order {OrderSpec} 
			self shouldNotImplement.
			^NULL "fodder"!
		*/
	}

	public int actualHashForEqual() {
		return ((getClass().hashCode() ^ (myTransitionVals.contentsHash())) ^ (myTransitionFlags.contentsHash())) ^ ((myStartsInside) ? 255 : 0);
		/*
		udanax-top.st:69315:RealRegion methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^((self getCategory hashForEqual
				bitXor: (myTransitionVals contentsHash))
				bitXor: (myTransitionFlags contentsHash))
				bitXor: (myStartsInside ifTrue: [255] ifFalse: [UInt32Zero])!
		*/
	}

	public boolean hasMember(Position position) {
		return TheManager.hasMember(this, position);
		/*
		udanax-top.st:69322:RealRegion methodsFor: 'testing'!
		{BooleanVar INLINE} hasMember: position {Position} 
			^TheManager hasMember: self with: position!
		*/
	}

	/**
	 * Same meaning as IntegerRegion::isBoundedAbove
	 */
	public boolean isBoundedAbove() {
		return TheManager.isBoundedRight(this);
		/*
		udanax-top.st:69326:RealRegion methodsFor: 'testing'!
		{BooleanVar CLIENT INLINE} isBoundedAbove
			"Same meaning as IntegerRegion::isBoundedAbove"
			
			^TheManager isBoundedRight: self!
		*/
	}

	/**
	 * Same meaning as IntegerRegion::isBoundedBelow
	 */
	public boolean isBoundedBelow() {
		return TheManager.isBoundedLeft(this);
		/*
		udanax-top.st:69331:RealRegion methodsFor: 'testing'!
		{BooleanVar CLIENT INLINE} isBoundedBelow
			"Same meaning as IntegerRegion::isBoundedBelow"
			
			^TheManager isBoundedLeft: self!
		*/
	}

	public boolean isEmpty() {
		return TheManager.isEmpty(this);
		/*
		udanax-top.st:69336:RealRegion methodsFor: 'testing'!
		{BooleanVar INLINE} isEmpty
			^TheManager isEmpty: self!
		*/
	}

	/**
	 * Any representable infinite set of real numbers is also not enumerable
	 */
	public boolean isEnumerable(OrderSpec order) {
		return isFinite();
		/*
		udanax-top.st:69340:RealRegion methodsFor: 'testing'!
		{BooleanVar INLINE} isEnumerable: order {OrderSpec default: NULL}
			"Any representable infinite set of real numbers is also not enumerable"
			^self isFinite!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof RealRegion) {
			RealRegion region = (RealRegion) other;
			return myStartsInside == region.startsInside() && (secretTransitions().contentsEqual(region.secretTransitions()));
		} else {
			return false;
		}
		/*
		udanax-top.st:69345:RealRegion methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: RealRegion into: [ :region |
				^myStartsInside == region startsInside
					and: [self secretTransitions contentsEqual: region secretTransitions]]
			others:
				[^false].
			^false "fodder"!
		*/
	}

	public boolean isFinite() {
		return TheManager.isFinite(this);
		/*
		udanax-top.st:69354:RealRegion methodsFor: 'testing'!
		{BooleanVar} isFinite
			^TheManager isFinite: self!
		*/
	}

	public boolean isFull() {
		return TheManager.isFull(this);
		/*
		udanax-top.st:69358:RealRegion methodsFor: 'testing'!
		{BooleanVar} isFull
			^TheManager isFull: self!
		*/
	}

	public boolean isSimple() {
		return TheManager.isSimple(this);
		/*
		udanax-top.st:69362:RealRegion methodsFor: 'testing'!
		{BooleanVar} isSimple
			^TheManager isSimple: self!
		*/
	}

	public boolean isSubsetOf(XnRegion other) {
		return TheManager.isSubsetOf(this, other);
		/*
		udanax-top.st:69366:RealRegion methodsFor: 'testing'!
		{BooleanVar} isSubsetOf: other {XnRegion}
			
			^TheManager isSubsetOf: self with: other!
		*/
	}

	public XnRegion complement() {
		return TheManager.complement(this);
		/*
		udanax-top.st:69372:RealRegion methodsFor: 'operations'!
		{XnRegion INLINE} complement
			^TheManager complement: self!
		*/
	}

	public XnRegion intersect(XnRegion other) {
		return TheManager.intersect(this, other);
		/*
		udanax-top.st:69376:RealRegion methodsFor: 'operations'!
		{XnRegion INLINE} intersect: other {XnRegion} 
			^TheManager intersect: self with: other!
		*/
	}

	public XnRegion simpleUnion(XnRegion other) {
		return TheManager.simpleUnion(this, other);
		/*
		udanax-top.st:69380:RealRegion methodsFor: 'operations'!
		{XnRegion} simpleUnion: other {XnRegion} 
			^TheManager simpleUnion: self with: other!
		*/
	}

	public XnRegion unionWith(XnRegion other) {
		return TheManager.unionWith(this, other);
		/*
		udanax-top.st:69384:RealRegion methodsFor: 'operations'!
		{XnRegion} unionWith: other {XnRegion} 
			^TheManager unionWith: self with: other!
		*/
	}

	public PtrArray secretTransitions() {
		PtrArray result;
		result = (PtrArray) (PrimSpec.pointer().array(myTransitionVals.count()));
		for (int i = 0; i < result.count(); i++) {
			RealPos pos;
			RealEdge edge;
			pos = RealPos.make((myTransitionVals.floatAt(i)));
			if (myTransitionFlags.integerAt(i).isEqual(IntegerValue.zero())) {
				edge = BeforeReal.make(pos);
			} else {
				edge = AfterReal.make(pos);
			}
			result.store(i, edge);
		}
		return result;
		/*
		udanax-top.st:69390:RealRegion methodsFor: 'secret'!
		{PtrArray of: RealEdge} secretTransitions
			| result {PtrArray of: RealEdge} |
			result := (PrimSpec pointer array: myTransitionVals count) cast: PtrArray.
			Int32Zero almostTo: result count do: [: i {Int32} |
				| pos {RealPos} edge {RealEdge} |
				pos := RealPos make: (myTransitionVals floatAt: i).
				(myTransitionFlags integerAt: i) = IntegerVar0 ifTrue:
					[edge := BeforeReal make: pos]
				ifFalse:
					[edge := AfterReal make: pos].
				result at: i store: edge].
			^result!
		*/
	}

	public boolean startsInside() {
		return myStartsInside;
		/*
		udanax-top.st:69404:RealRegion methodsFor: 'secret'!
		{BooleanVar INLINE} startsInside
			^myStartsInside!
		*/
	}

	public void printOn(PrintWriter oo) {
		TheManager.printRegionOn(this, oo);
		/*
		udanax-top.st:69410:RealRegion methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			TheManager printRegionOn: self with: oo!
		*/
	}

	public XnRegion asSimpleRegion() {
		return TheManager.asSimpleRegion(this);
		/*
		udanax-top.st:69416:RealRegion methodsFor: 'accessing'!
		{XnRegion INLINE} asSimpleRegion
			^TheManager asSimpleRegion: self!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return RealSpace.make();
		/*
		udanax-top.st:69420:RealRegion methodsFor: 'accessing'!
		{CoordinateSpace INLINE} coordinateSpace
			
			^RealSpace make!
		*/
	}

	/**
	 * The largest real number such that all the positions in the region are >= it.  Does not
	 * necessarily lie in the region.  For example, the region of all numbers > 2 has a
	 * lowerBound of 2.
	 */
	public RealPos lowerBound() {
		return (RealPos) (TheManager.greatestLowerBound(this));
		/*
		udanax-top.st:69424:RealRegion methodsFor: 'accessing'!
		{RealPos CLIENT} lowerBound
			"The largest real number such that all the positions in the region are >= it.  Does not necessarily lie in the region.  For example, the region of all numbers > 2 has a lowerBound of 2."
			^(TheManager greatestLowerBound: self) cast: RealPos!
		*/
	}

	/**
	 * The smallest real number such that all the positions in the region are <= it.  Does not
	 * necessarily lie in the region.  For example, the region of all numbers < 2 has an
	 * upperBound of 2.
	 */
	public RealPos upperBound() {
		return (RealPos) (TheManager.leastUpperBound(this));
		/*
		udanax-top.st:69429:RealRegion methodsFor: 'accessing'!
		{RealPos CLIENT} upperBound
			"The smallest real number such that all the positions in the region are <= it.  Does not necessarily lie in the region.  For example, the region of all numbers < 2 has an upperBound of 2."
			^(TheManager leastUpperBound: self) cast: RealPos!
		*/
	}

	public RealRegion(boolean startsInside, PrimFloatArray vals, PrimIntegerArray flags) {
		super();
		myStartsInside = startsInside;
		myTransitionVals = vals;
		myTransitionFlags = flags;
		/*
		udanax-top.st:69436:RealRegion methodsFor: 'creation'!
		create: startsInside {BooleanVar} with: vals {PrimFloatArray} with: flags {PrimIntegerArray}
			super create.
			myStartsInside := startsInside.
			myTransitionVals := vals.
			myTransitionFlags := flags!
		*/
	}

	public Stepper intervals() {
		return intervals(null);
		/*
		udanax-top.st:69445:RealRegion methodsFor: 'smalltalk: defaults'!
		{Stepper CLIENT of: RealRegion} intervals
			^self intervals: NULL!
		*/
	}

	public RealRegion(Rcvr receiver) {
		super(receiver);
		myStartsInside = receiver.receiveBooleanVar();
		myTransitionVals = (PrimFloatArray) receiver.receiveHeaper();
		myTransitionFlags = (PrimIntegerArray) receiver.receiveHeaper();
		/*
		udanax-top.st:69450:RealRegion methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myStartsInside _ receiver receiveBooleanVar.
			myTransitionVals _ receiver receiveHeaper.
			myTransitionFlags _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendBooleanVar(myStartsInside);
		xmtr.sendHeaper(myTransitionVals);
		xmtr.sendHeaper(myTransitionFlags);
		/*
		udanax-top.st:69456:RealRegion methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendBooleanVar: myStartsInside.
			xmtr sendHeaper: myTransitionVals.
			xmtr sendHeaper: myTransitionFlags.!
		*/
	}

	//	public static void initTimeNonInherited() {
	//		REQUIRES(EdgeManager.getCategory());
	//		TheManager = new RealManager();
	//		/*
	//		udanax-top.st:69471:RealRegion class methodsFor: 'smalltalk: initialization'!
	//		initTimeNonInherited
	//			
	//			self REQUIRES: EdgeManager.
	//			TheManager := RealManager create.!
	//		*/
	//	}

	//	public static void linkTimeNonInherited() {
	//		TheManager = null;
	//		/*
	//		udanax-top.st:69476:RealRegion class methodsFor: 'smalltalk: initialization'!
	//		linkTimeNonInherited
	//			
	//			TheManager := NULL.!
	//		*/
	//	}

	/**
	 * Make a new region, reusing the given array. Noone else should ever modify it!!
	 */
	public static RealRegion make(boolean startsInside, PrimArray transitions) {
		int precision;
		PrimSpec spec;
		PrimFloatArray transVals;
		PrimIntegerArray transFlags;
		PtrArray tr;
		tr = (PtrArray) transitions;
		precision = 0;
		for (int i = 0; i < transitions.count(); i++) {
			precision = Math.max(precision, ((RealEdge) (tr.get(i))).position().precision());
		}
		if (precision == 64) {
			spec = PrimSpec.iEEE64();
		} else {
			if (precision == 32) {
				spec = PrimSpec.iEEE32();
			} else {
				if (precision == 8) {
					spec = PrimSpec.iEEE32();
					//					TODO thingToDo(); /* add an iEEE8 spec to the system and use it here */;
				} else {
					if (transitions.count() == 0) {
						spec = PrimSpec.iEEE64();
					} else {
						throw new AboraRuntimeException(AboraRuntimeException.UNRECOGNIZED_PRECISION);
					}
				}
			}
		}
		transVals = (PrimFloatArray) (spec.array(transitions.count()));
		transFlags = (PrimIntegerArray) (PrimSpec.uInt8().array(tr.count()));
		//		TODO thingToDo(); /* add 'PrimSpec uInt1' to the system and use it here */
		for (int i = 0; i < transitions.count(); i++) {
			RealEdge edge;
			IntegerValue flag;
			edge = ((RealEdge) (tr.get(i)));
			transVals.storeFloat(i, edge.position().asIEEE64());
			if (edge instanceof BeforeReal) {
				BeforeReal after = (BeforeReal) edge;
				flag = IntegerValue.zero();
			} else if (edge instanceof AfterReal) {
				AfterReal before = (AfterReal) edge;
				flag = IntegerValue.one();
			} else {
				throw new IllegalStateException();
			}
			transFlags.storeInteger(i, flag);
		}
		return usingx(startsInside, transVals, transFlags);
		/*
		udanax-top.st:69482:RealRegion class methodsFor: 'creation'!
		{RealRegion} make: startsInside {BooleanVar}
			with: transitions {PrimArray of: RealEdge}
			"Make a new region, reusing the given array. Noone else should ever modify it!!"
			| precision {Int32} spec {PrimSpec} transVals {PrimFloatArray} transFlags {PrimIntegerArray} tr {PtrArray} |
			tr := transitions cast: PtrArray.
			precision := Int32Zero.
			Int32Zero almostTo: transitions count do: [:i {Int32} |
				precision := precision max: ((tr get: i) cast: RealEdge) position precision].
			precision = 64 ifTrue:
				[spec := PrimSpec iEEE64]
			ifFalse: [precision = 32 ifTrue:
				[spec := PrimSpec iEEE32]
			ifFalse: [precision = 8 ifTrue:
				[spec := PrimSpec iEEE32.
				self thingToDo. "add an iEEE8 spec to the system and use it here"]
			ifFalse: [transitions count = Int32Zero ifTrue:
				[spec := PrimSpec iEEE64]
			ifFalse:
				[Heaper BLAST: #UnrecognizedPrecision]]]].
			transVals := (spec array: transitions count) cast: PrimFloatArray.
			transFlags := (PrimSpec uInt8 array: tr count) cast: PrimIntegerArray.
			self thingToDo. "add 'PrimSpec uInt1' to the system and use it here"
			
			Int32Zero almostTo: transitions count do: [:i {Int32} |
				| edge {RealEdge} flag {UInt1} |
				edge := ((tr get: i) cast: RealEdge).
				transVals at: i storeFloat: edge position asIEEE64.
				edge
					cast: BeforeReal into: [:after | flag := UInt32Zero]
					cast: AfterReal into: [:before | flag := 1].
				transFlags at: i storeInteger: flag].
				
			^self usingx: startsInside with: transVals with: transFlags!
		*/
	}

	public static RealRegion usingx(boolean startsInside, PrimFloatArray vals, PrimIntegerArray flags) {
		return new RealRegion(startsInside, vals, flags);
		/*
		udanax-top.st:69517:RealRegion class methodsFor: 'creation'!
		{RealRegion} usingx: startsInside {BooleanVar} with: vals {PrimFloatArray} with: flags {PrimIntegerArray}
			^self create: startsInside with: vals with: flags!
		*/
	}

	/**
	 * {Stepper CLIENT of: RealRegion} intervals: order {OrderSpec default: NULL}
	 * {BooleanVar CLIENT} isBoundedAbove
	 * {BooleanVar CLIENT} isBoundedBelow
	 * {BooleanVar CLIENT} isInterval
	 * {XuReal CLIENT} lowerBound
	 * {XuReal CLIENT} upperBound
	 */
	public static void info() {
		/*
		udanax-top.st:69523:RealRegion class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{Stepper CLIENT of: RealRegion} intervals: order {OrderSpec default: NULL}
		{BooleanVar CLIENT} isBoundedAbove
		{BooleanVar CLIENT} isBoundedBelow
		{BooleanVar CLIENT} isInterval
		{XuReal CLIENT} lowerBound
		{XuReal CLIENT} upperBound
		"!
		*/
	}
}
