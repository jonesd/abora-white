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
package org.abora.white.collection.sets;

import org.abora.white.collection.steppers.Stepper;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * ImmuSets are ScruSets which are guaranteed never to change.  ImmuSets correspond to the
 * mathematical notion of a finite set of elements, except of course that here the elements
 * can be any valid X++ object.  Just like mathematical sets, two are equal (according to
 * isEqual) iff they have the same elements.  Just because the set cannot change, that
 * doesn''t prevent any of the members from undergoing state change.
 * ImmuSets implement some additional protocol to make new sets out of old ones according to
 * the familiar set theoretic operators (like intersect).  XuRegions are much like ImmuSets
 * of Positions except that they aren''t necessarily finite or even enumerable.  XuRegions
 * implement a similar protocol, but aren''t polymorphic with ImmuSets.
 */
public abstract class ImmuSet extends ScruSet {
	protected static ImmuSet EmptySet = new EmptyImmuSet();
	/*
	udanax-top.st:45372:
	ScruSet subclass: #ImmuSet
		instanceVariableNames: ''
		classVariableNames: 'EmptySet {ImmuSet wimpy} '
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:45376:
	ImmuSet comment:
	'ImmuSets are ScruSets which are guaranteed never to change.  ImmuSets correspond to the mathematical notion of a finite set of elements, except of course that here the elements can be any valid X++ object.  Just like mathematical sets, two are equal (according to isEqual) iff they have the same elements.  Just because the set cannot change, that doesn''t prevent any of the members from undergoing state change.
		
		ImmuSets implement some additional protocol to make new sets out of old ones according to the familiar set theoretic operators (like intersect).  XuRegions are much like ImmuSets of Positions except that they aren''t necessarily finite or even enumerable.  XuRegions implement a similar protocol, but aren''t polymorphic with ImmuSets. '!
	*/
	/*
	udanax-top.st:45380:
	(ImmuSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:45455:
	ImmuSet class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:45458:
	(ImmuSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected ImmuSet() {
		super();
	}

	protected ImmuSet(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Accessing

	public abstract boolean hasMember(Heaper someone);
	/*
	udanax-top.st:45385:ImmuSet methodsFor: 'accessing'!
	{BooleanVar} hasMember: someone {Heaper}
		self subclassResponsibility!
	*/

	public abstract boolean isEmpty();
	/*
	udanax-top.st:45388:ImmuSet methodsFor: 'accessing'!
	{BooleanVar} isEmpty
		self subclassResponsibility!
	*/

	/**
	 * Regular set intersection.  Return an ImmuSet containing only those objects which are
	 * members of
	 * both sets
	 */
	public abstract ImmuSet intersect(ScruSet another);
	/*
	udanax-top.st:45393:ImmuSet methodsFor: 'operations'!
	{ImmuSet} intersect: another {ScruSet} 
		"Regular set intersection.  Return an ImmuSet containing only those objects which are members of 
		both sets"
		self subclassResponsibility!
	*/

	/**
	 * Return an ImmuSet containing those of my members which aren't members of 'another'
	 */
	public abstract ImmuSet minus(ScruSet another);
	/*
	udanax-top.st:45398:ImmuSet methodsFor: 'operations'!
	{ImmuSet} minus: another {ScruSet}
		"Return an ImmuSet containing those of my members which aren't members of 'another'"
		self subclassResponsibility!
	*/

	/**
	 * Return an ImmuSet containing those objects with are members of either of us
	 */
	public abstract ImmuSet unionWith(ScruSet another);
	/*
	udanax-top.st:45402:ImmuSet methodsFor: 'operations'!
	{ImmuSet} unionWith: another {ScruSet}
		"Return an ImmuSet containing those objects with are members of either of us"
		self subclassResponsibility!
	*/

	/**
	 * 'set->with (anElement)' means the same as 'set->unionWith (immuSet (anElement))'.
	 * It returns an ImmuSet with all my members and having anElement as a member.
	 * If anElement is a member of me, then the result is identical to me.
	 */
	public abstract ImmuSet with(Heaper anElement);
	/*
	udanax-top.st:45408:ImmuSet methodsFor: 'adding-removing'!
	{ImmuSet} with: anElement {Heaper}
		"'set->with (anElement)' means the same as 'set->unionWith (immuSet (anElement))'.
		It returns an ImmuSet with all my members and having anElement as a member.
		If anElement is a member of me, then the result is identical to me."
		self subclassResponsibility!
	*/

	/**
	 * 'set->without (anElement)' means the same as 'set->minus (immuSet (anElement))'.
	 * It returns an ImmuSet with all my members except anElement.  If anElement isn't already a
	 * member,
	 * then the result is identical to me.
	 */
	public abstract ImmuSet without(Heaper anElement);
	/*
	udanax-top.st:45414:ImmuSet methodsFor: 'adding-removing'!
	{ImmuSet} without: anElement {Heaper}
		"'set->without (anElement)' means the same as 'set->minus (immuSet (anElement))'.
		It returns an ImmuSet with all my members except anElement.  If anElement isn't already a member,
		then the result is identical to me."
		self subclassResponsibility!
	*/

	/**
	 * don't need to actually make a copy, as this is immutable
	 */
	public ScruSet copy() {
		return this;
		/*
		udanax-top.st:45422:ImmuSet methodsFor: 'creation'!
		{ScruSet} copy
			"don't need to actually make a copy, as this is immutable"
			^self!
		*/
	}

	public ImmuSet asImmuSet() {
		return this;
		/*
		udanax-top.st:45428:ImmuSet methodsFor: 'conversion'!
		{ImmuSet} asImmuSet
			^self!
		*/
	}

	public abstract MuSet asMuSet();
	/*
	udanax-top.st:45431:ImmuSet methodsFor: 'conversion'!
	{MuSet} asMuSet
		self subclassResponsibility!
	*/

	public abstract IntegerValue count();
	/*
	udanax-top.st:45436:ImmuSet methodsFor: 'enumerating'!
	{IntegerVar} count
		self subclassResponsibility!
	*/

	public abstract Stepper stepper();
	/*
	udanax-top.st:45439:ImmuSet methodsFor: 'enumerating'!
	{Stepper} stepper
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return contentsHash();
		/*
		udanax-top.st:45444:ImmuSet methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^self contentsHash!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof ImmuSet) {
			ImmuSet o = (ImmuSet) other;
			return contentsEqual(o);
		} else {
			return false;
		}
		/*
		udanax-top.st:45447:ImmuSet methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper} 
			other cast: ImmuSet into: [:o |
					^self contentsEqual: o]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	//	public static ImmuSet create(Heaper ms) {
	//		return new ImmuSet(ms);
	//		/*
	//		udanax-top.st:45463:ImmuSet class methodsFor: 'smalltalk: constructors'!
	//		create.MuSet: ms
	//			^ self new create.MuSet: ms!
	//		*/
	//	}

	//	public static ImmuSet make(Heaper thing) {
	//		if (thing instanceof XnRegion) {
	//			throw new IllegalArgumentException("Use region convert: ImmuSet");
	////			return ImmuSet.make(thing);
	//		}
	//		return ImmuSet.make(((MuSet) thing));
	//		/*
	//		udanax-top.st:45466:ImmuSet class methodsFor: 'smalltalk: constructors'!
	//		{ImmuSet} make: thing
	//			(thing isKindOf: XnRegion)
	//				ifTrue: [false assert: 'Use region convert: ImmuSet'.
	//						^ImmuSet make.XuRegion: thing].
	//			^ImmuSet make.MuSet: (thing cast: MuSet)!
	//		*/
	//	}

	/**
	 * This is for ImmuSet subclasses to produce results from temporary MuSets.
	 * The difference between this and ImmuSet make.MuSet: is that this doesn't make a copy
	 * of the MuSet when making an ImmuSetOnMu.
	 */
	public static ImmuSet from(MuSet set) {
		if (set.isEmpty()) {
			return EmptySet;
		}
		if (set.count().isEqual(IntegerValue.one())) {
			return TinyImmuSet.make(set.theOne());
		}
		return ImmuSetOnMu.make(set);
		/*
		udanax-top.st:45474:ImmuSet class methodsFor: 'protected: pseudo constructors'!
		{ImmuSet} from: set {MuSet}
			"This is for ImmuSet subclasses to produce results from temporary MuSets.
			The difference between this and ImmuSet make.MuSet: is that this doesn't make a copy
			of the MuSet when making an ImmuSetOnMu."
			set isEmpty ifTrue: [^ EmptySet].
			set count == 1 ifTrue: [^ TinyImmuSet make: set theOne].
			^ ImmuSetOnMu make: set!
		*/
	}

	public static ImmuSet make() {
		return EmptySet;
		/*
		udanax-top.st:45484:ImmuSet class methodsFor: 'pseudo constructors'!
		{ImmuSet INLINE} make
			^EmptySet!
		*/
	}

	public static ImmuSet make(MuSet set) {
		if (set.isEmpty()) {
			return EmptySet;
		}
		if (set.count().isEqual(IntegerValue.one())) {
			return TinyImmuSet.make(set.theOne());
		}
		return ImmuSetOnMu.make(((MuSet) set.copy()));
		/*
		udanax-top.st:45487:ImmuSet class methodsFor: 'pseudo constructors'!
		{ImmuSet} make.MuSet: set {MuSet}
			set isEmpty ifTrue: [^ EmptySet].
			set count == 1 ifTrue: [^ TinyImmuSet make: set theOne].
			^ ImmuSetOnMu make: (set copy cast: MuSet).!
		*/
	}

	/**
	 * A single element ImmuSet
	 */
	public static ImmuSet newWith(Heaper value) {
		return TinyImmuSet.make(value);
		/*
		udanax-top.st:45493:ImmuSet class methodsFor: 'pseudo constructors'!
		{ImmuSet} newWith: value {Heaper}
			"A single element ImmuSet"
			
			^TinyImmuSet make: value!
		*/
	}

	//public static void initTimeNonInherited() {
	//REQUIRES(Stepper.getCategory());
	//EmptySet = (EmptyImmuSet.new(PERSISTENT)) new ImmuSet();
	///*
	//udanax-top.st:45500:ImmuSet class methodsFor: 'smalltalk: initialization'!
	//initTimeNonInherited
	//	self REQUIRES: Stepper.
	//	EmptySet _ (EmptyImmuSet new.AllocType: #PERSISTENT) create.!
	//*/
	//}

	//public static void linkTimeNonInherited() {
	//EmptySet = null;
	///*
	//udanax-top.st:45504:ImmuSet class methodsFor: 'smalltalk: initialization'!
	//linkTimeNonInherited
	//	EmptySet _ NULL!
	//*/
	//}

	//public static Heaper make(Heaper aSingleton) {
	//passe();
	///* use ImmuSet make with: aSingleton */
	//;
	///*
	//udanax-top.st:45510:ImmuSet class methodsFor: 'smalltalk: passe'!
	//make.Heaper: aSingleton {Heaper}
	//	self passe. "use ImmuSet make with: aSingleton"!
	//*/
	//}

	///**
	// * A single element ImmuSet
	// */
	//public static ImmuSet with(Heaper value) {
	//passe();
	///* use newWith: */
	//;
	///*
	//udanax-top.st:45514:ImmuSet class methodsFor: 'smalltalk: passe'!
	//{ImmuSet} with: value {Heaper}
	//	"A single element ImmuSet"
	//	
	//	self passe. "use newWith:"!
	//*/
	//}
}
