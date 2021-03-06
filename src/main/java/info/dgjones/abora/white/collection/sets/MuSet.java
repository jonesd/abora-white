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
package info.dgjones.abora.white.collection.sets;

import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * MuSets are a changable collection of elements.  Added to the ScruSet protocol are messages
 * for performing these changes.  The "introduce/store/wipe/remove" suite is defined by
 * analogy with similar methods in MuTable.  See both ScruSet and MuTable.
 */
public abstract class MuSet extends ScruSet {
	/*
	udanax-top.st:45823:
	ScruSet subclass: #MuSet
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:45827:
	MuSet comment:
	'MuSets are a changable collection of elements.  Added to the ScruSet protocol are messages for performing these changes.  The "introduce/store/wipe/remove" suite is defined by analogy with similar methods in MuTable.  See both ScruSet and MuTable.'!
	*/
	/*
	udanax-top.st:45829:
	(MuSet getOrMakeCxxClassDescription)
		friends:
	'/- friends for class MuSet -/
	friend class ImmuSetOnMu;
	friend class COWMuSet;';
		attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
	*/
	/*
	udanax-top.st:45926:
	MuSet class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:45929:
	(MuSet getOrMakeCxxClassDescription)
		friends:
	'/- friends for class MuSet -/
	friend class ImmuSetOnMu;
	friend class COWMuSet;';
		attributes: ((Set new) add: #DEFERRED; add: #EQ; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected MuSet() {
		super();
	}

	protected MuSet(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	public static MuSet fromStepper(Stepper stepper) {
		MuSet result = MuSet.make();
		try {
			Heaper element;
			while ((element = (Heaper) stepper.fetch()) != null) {
				result.store(element);
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:45949:MuSet class methodsFor: 'pseudo constructors'!
		{MuSet} fromStepper: stepper {Stepper}
			| result {MuSet} |
			result _ MuSet make.
			stepper forEach: [ :element {Heaper} |
				result store: element].
			^result!
		*/
	}

	public static MuSet make() {
		return ActualHashSet.make();
		/*
		udanax-top.st:45956:MuSet class methodsFor: 'pseudo constructors'!
		{MuSet} make 
			^ActualHashSet make!
		*/
	}

	public static MuSet make(Heaper item) {
		return ActualHashSet.make(item);
		/*
		udanax-top.st:45959:MuSet class methodsFor: 'pseudo constructors'!
		{MuSet} make.Heaper: item {Heaper}
			^ActualHashSet make.Heaper: item!
		*/
	}

	/**
	 * someSize is a non-semantic hint about how big the set might get.
	 */
	public static MuSet make(IntegerValue someSize) {
		return ActualHashSet.make(someSize);
		/*
		udanax-top.st:45963:MuSet class methodsFor: 'pseudo constructors'!
		{MuSet} make.IntegerVar: someSize {IntegerVar}
			"someSize is a non-semantic hint about how big the set might get."
			^ActualHashSet make.IntegerVar: someSize!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public abstract boolean hasMember(Heaper someone);
	/*
	udanax-top.st:45838:MuSet methodsFor: 'accessing'!
	{BooleanVar} hasMember: someone {Heaper}
		self subclassResponsibility!
	*/

	public abstract boolean isEmpty();
	/*
	udanax-top.st:45841:MuSet methodsFor: 'accessing'!
	{BooleanVar} isEmpty
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Operations

	/**
	 * Sort of intersect.  Wipe from myself all elements that I don't have in common with other.
	 * Turn myself into the intersection of my current self and other.
	 */
	public void restrictTo(ScruSet other) {
		MuSet tmp = (MuSet) copy();
		try {
			tmp.wipeAll(other);
			wipeAll(tmp);
		} finally {
			tmp.destroy();
		}
		/*
		udanax-top.st:45846:MuSet methodsFor: 'operations'!
		{void} restrictTo: other {ScruSet} 
			"Sort of intersect.  Wipe from myself all elements that I don't have in common with other.
			Turn myself into the intersection of my current self and other."
			| tmp {MuSet} |
			tmp _ self copy cast: MuSet.
			tmp wipeAll: other.
			self wipeAll: tmp.
			tmp destroy.!
		*/
	}

	/**
	 * Sort of union.  Store into myself all elements from other.
	 * Turn myself into the union of my current self and other.
	 */
	public void storeAll(ScruSet other) {
		Stepper stepper = other.stepper();
		try {
			Heaper elem;
			while ((elem = (Heaper) stepper.fetch()) != null) {
				store(elem);
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		/*
		udanax-top.st:45856:MuSet methodsFor: 'operations'!
		{void} storeAll: other {ScruSet} 
			"Sort of union.  Store into myself all elements from other.
			Turn myself into the union of my current self and other."
			other stepper forEach: [:elem {Heaper wimpy} | self store: elem]!
		*/
	}

	/**
	 * Sort of minus.  Wipe from myself all elements from other.
	 * Turn myself into my current self minus other.
	 */
	public void wipeAll(ScruSet other) {
		Stepper stepper = other.stepper();
		try {
			Heaper elem;
			while ((elem = (Heaper) stepper.fetch()) != null) {
				wipe(elem);
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		/*
		udanax-top.st:45862:MuSet methodsFor: 'operations'!
		{void} wipeAll: other {ScruSet} 
			"Sort of minus.  Wipe from myself all elements from other.
			Turn myself into my current self minus other."
			other stepper forEach: [:elem {Heaper wimpy} | self wipe: elem]!
		*/
	}

	/////////////////////////////////////////////
	// Adding-Removing

	/**
	 * Add anElement to my members, but only if it isn't already a member.
	 * If it is already a member, BLAST
	 */
	public abstract void introduce(Heaper anElement);
	/*
	udanax-top.st:45870:MuSet methodsFor: 'adding-removing'!
	{void} introduce: anElement {Heaper}
		"Add anElement to my members, but only if it isn't already a member.
		If it is already a member, BLAST"
		
		self subclassResponsibility!
	*/

	/**
	 * Remove anElement from my members.  If it isn't currently a member, then BLAST
	 */
	public abstract void remove(Heaper anElement);
	/*
	udanax-top.st:45876:MuSet methodsFor: 'adding-removing'!
	{void} remove: anElement {Heaper}
		"Remove anElement from my members.  If it isn't currently a member, then BLAST"
		
		self subclassResponsibility!
	*/

	/**
	 * Add anElement to my set of members.  No semantic effect if anElement is already a member.
	 */
	public abstract void store(Heaper anElement);
	/*
	udanax-top.st:45881:MuSet methodsFor: 'adding-removing'!
	{void} store: anElement {Heaper}
		"Add anElement to my set of members.  No semantic effect if anElement is already a member."
		
		self subclassResponsibility!
	*/

	/**
	 * make anElement no longer be one of my members.  No semantic effect if it already isn't a
	 * member.
	 */
	public abstract void wipe(Heaper anElement);
	/*
	udanax-top.st:45886:MuSet methodsFor: 'adding-removing'!
	{void} wipe: anElement {Heaper}
		"make anElement no longer be one of my members.  No semantic effect if it already isn't a member."
		
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Creation

	public abstract ScruSet copy();
	/*
	udanax-top.st:45893:MuSet methodsFor: 'creation'!
	{ScruSet} copy
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Conversion

	public ImmuSet asImmuSet() {
		if (isEmpty()) {
			return ImmuSet.make();
		}
		if (count().isEqual(IntegerValue.one())) {
			return ImmuSet.make().with(theOne());
		}
		return ImmuSet.make(this);
		/*
		udanax-top.st:45898:MuSet methodsFor: 'conversion'!
		{ImmuSet} asImmuSet
			self isEmpty ifTrue: [^ ImmuSet make].
			self count == 1 ifTrue: [^ ImmuSet make with: (self theOne)].
			^ ImmuSet make: self!
		*/
	}

	public MuSet asMuSet() {
		return (MuSet) copy();
		/*
		udanax-top.st:45903:MuSet methodsFor: 'conversion'!
		{MuSet} asMuSet
			^ self copy quickCast: MuSet!
		*/
	}

	/////////////////////////////////////////////
	// Enumerating

	public abstract IntegerValue count();
	/*
	udanax-top.st:45908:MuSet methodsFor: 'enumerating'!
	{IntegerVar} count
		self subclassResponsibility!
	*/

	public abstract Stepper stepper();
	/*
	udanax-top.st:45911:MuSet methodsFor: 'enumerating'!
	{Stepper} stepper
		self subclassResponsibility!
	*/

	protected abstract Stepper immuStepper();
	/*
	udanax-top.st:45916:MuSet methodsFor: 'private: enumerating'!
	{Stepper} immuStepper
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Comparing and Hashing

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//TODOreturn asOop();
		/*
		udanax-top.st:45921:MuSet methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:45923:MuSet methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}

	//	/**
	//	 * someSize is a non-semantic hint about how big the set might get.
	//	 */
	//	public static Heaper make(XnRegion region) {
	//		MuSet result;
	//		passe();
	//		result = ActualHashSet.make(region.count());
	//		for (Iterator iterator = region.stepper().forEach(); iterator.hasNext();) {
	//			Position position = (Position) iterator.next();
	//			result.store(position);
	//		}
	//		return result;
	//		/*
	//		udanax-top.st:45938:MuSet class methodsFor: 'smalltalk: passe'!
	//		make.Region: region {XnRegion} 
	//			"someSize is a non-semantic hint about how big the set might get."
	//			| result {MuSet} |
	//			self passe.
	//			result _ ActualHashSet make.IntegerVar: region count.
	//			region stepper forEach: [:position {Position} | result store: position].
	//			^result!
	//		*/
	//	}


//	public static MuSet make(Object something) {
//		if (something instanceof Integer) {
//			return make(something);
//		}
//		return make(((XnRegion) something));
//		/*
//		udanax-top.st:45969:MuSet class methodsFor: 'smalltalk: defaults'!
//		make: something
//			(something isKindOf: Integer) ifTrue:
//				[^self make.IntegerVar: something].
//			^self make.Region: (something cast: XnRegion)!
//		*/
//	}

	//	public static void initTimeNonInherited() {
	//		REQUIRES(ActualHashSet.getCategory());
	//		/*
	//		udanax-top.st:45976:MuSet class methodsFor: 'smalltalk: initialization'!
	//		initTimeNonInherited
	//			self REQUIRES: ActualHashSet!
	//		*/
	//	}

//	public static void problems() {
//		throw new UnsupportedOperationException();
//		//return signals((ALREADY_IN_SET);
//		/*
//		udanax-top.st:45982:MuSet class methodsFor: 'exceptions: exceptions'!
//		problems.AlreadyInSet
//			^self signals: #(AlreadyInSet)!
//		*/
//	}
}
