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

import java.io.PrintWriter;

import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * X++ has three basic kinds of collection classes.  Tables, Sets and XuRegions.  XuRegions
 * are not-necessarily-discrete collections of positions, and are documented in the space
 * module.  Sets and Tables are both discrete and finite, and similar in many ways.  Both
 * originate in a three-way type distinction between:
 * <ul>
 * <li>ScruX  --  The protocol for examining one.  I.e., it is *Scru*table</li>
 * <li>ImmuX  --  The contract guarantees that the set or table you''re looking at won't change
 * (though the things it contains may change)</li>
 * <li>MuX  --  Additional protocol for changing it.</li>
 * </ul>
 * Concrete classes may be a subclass of any of the above.  It makes sense to have a concrete
 * subclass of ScruX which isn''t a subclass of either MuX or ImmuX when, for example, it
 * represents a tracking, filtered view of some other set which is itself changing.  All
 * kinds of collection can be iterated over when appropriate using Steppers--our basic
 * iteration abstraction (see Stepper).
 * <p>
 * Immu's are sort of like Stamps -- they represent a particular state a colection can have.
 * <p>
 * Mu's are sort of like Berts -- they represent a continuing collection identity which can
 * change its current state.
 * <p>
 * Sets are pure collections--their contents are just a set of Heapers.  Sets (as opposed to
 * tables) do not provide any organization of these contents.
 */
public abstract class ScruSet extends Heaper {
	/*
	udanax-top.st:45146:
	Heaper subclass: #ScruSet
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:45150:
	ScruSet comment:
	'X++ has three basic kinds of collection classes.  Tables, Sets and XuRegions.  XuRegions are not-necessarily-discrete collections of positions, and are documented in the space module.  Sets and Tables are both discrete and finite, and similar in many ways.  Both originate in a three-way type distinction between:
		
		ScruX  --  The protocol for examining one.  I.e., it is *Scru*table
			ImmuX  --  The contract guarantees that the set or table you''re looking at won''t change (though the things it contains may change)
			MuX  --  Additional protocol for changing it.
			
		Concrete classes may be a subclass of any of the above.  It makes sense to have a concrete subclass of ScruX which isn''t a subclass of either MuX or ImmuX when, for example, it represents a tracking, filtered view of some other set which is itself changing.  All kinds of collection can be iterated over when appropriate using Steppers--our basic iteration abstraction (see Stepper).
		
		Immu''s are sort of like Stamps -- they represent a particular state a colection can have.  Mu''s are sort of like Berts -- they represent a continuing collection identity which can change its current state.
		
		Sets are pure collections--their contents are just a set of Heapers.  Sets (as opposed to tables) do not provide any organization of these contents.'!
	*/
	/*
	udanax-top.st:45162:
	(ScruSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:45361:
	ScruSet class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:45364:
	(ScruSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected ScruSet() {
		super();
	}

	protected ScruSet(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Accessing

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//TODO return Heaper.takeOop();
		/*
		udanax-top.st:45167:ScruSet methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}


	/////////////////////////////////////////////
	// Testing
	
	/**
	 * Returns whether the two ScruSets have exactly the same set of elements at the moment.
	 * 'a->contentsEqual(b)' is equivalent to
	 * 'a->asImmuSet()->isEqual(b->asImmuSet())'.
	 */
	public boolean contentsEqual(ScruSet other) {
		if (!other.count().isEqual(count())) {
			return false;
		}
		Stepper stepper = other.stepper();
		try {
			Heaper each;
			while ((each = (Heaper) stepper.fetch()) != null) {
				if (!(hasMember(each))) {
					return false;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return true;
		/*
		udanax-top.st:45170:ScruSet methodsFor: 'testing'!
		{BooleanVar} contentsEqual: other {ScruSet}
			"Returns whether the two ScruSets have exactly the same set of elements at the moment.
			'a->contentsEqual(b)' is equivalent to 
			'a->asImmuSet()->isEqual(b->asImmuSet())'."
			
			other count ~= self count ifTrue: [^false].
			other stepper forEach: [ :each {Heaper} |
				(self hasMember: each) ifFalse: [^false]].
			^true!
		*/
	}

	/**
	 * Has the same relationship to contentsEqual that hashForEqual has to isEqual.
	 * I.e., if 'a->contentsEqual (b)', then 'a->contentsHash() == b->contentsHash()'.
	 * The same complex caveats apply as to the stability and portability of the
	 * hash values as apply for hashForEqual.
	 */
	public int contentsHash() {
		int result = 0;
		Stepper stepper = stepper();
		try {
			Heaper each;
			while ((each = (Heaper) stepper.fetch()) != null) {
				result ^= each.hashForEqual();
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:45180:ScruSet methodsFor: 'testing'!
		{UInt32} contentsHash
			"Has the same relationship to contentsEqual that hashForEqual has to isEqual. 
			I.e., if 'a->contentsEqual (b)', then 'a->contentsHash() == b->contentsHash()'. 
			The same complex caveats apply as to the stability and portability of the 
			hash values as apply for hashForEqual."
			| result {UInt32} |
			result _ UInt32Zero.
			self stepper forEach: [ :each {Heaper} |
				result _ result bitXor: each hashForEqual].
			^result!
		*/
	}

	/**
	 * Is someone a member of the set now?
	 */
	public abstract boolean hasMember(Heaper someone);
	/*
	udanax-top.st:45192:ScruSet methodsFor: 'testing'!
	{BooleanVar} hasMember: someone {Heaper}
		"Is someone a member of the set now?"
		self subclassResponsibility!
	*/

	/**
	 * Return true if <code>this</code> set and the <code>other</code>
	 * set have any elements in common.
	 * <p>
	 * Subclasses may want to override for efficiency.
	 * 
	 * @param other other set to test for an intersection with
	 * @return true if there are any elements in common 
	 */
	public boolean intersects(ScruSet other) {
		if (other.isEmpty()) {
			return false;
		}
		if (count().isGT(other.count())) {
			return other.intersects(this);
		}
		Stepper stepper = stepper();
		try {
			Heaper mem;
			while ((mem = (Heaper) stepper.fetch()) != null) {
				if (other.hasMember(mem)) {
					return true;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return false;
		/*
		udanax-top.st:45196:ScruSet methodsFor: 'testing'!
		{BooleanVar} intersects: other {ScruSet}
			"tell whether they have any points in common"
			"subclasses can override for efficiency"
			other isEmpty ifTrue: [ ^ false ].
			self count > other count
				ifTrue: [^other intersects: self].
			self stepper forEach: [:mem {Heaper} |
				(other hasMember: mem) ifTrue: [^true]].
			^false!
		*/
	}

	/**
	 * Return true if <code>this</code> set does not currently have
	 * any elements.
	 * 
	 * @return true if <code>this</code> set does not currently have any
	 * 	elements.
	 */
	public abstract boolean isEmpty();
	/*
	udanax-top.st:45207:ScruSet methodsFor: 'testing'!
	{BooleanVar} isEmpty
		"Whether it currently has any elements"
		self subclassResponsibility!
	*/

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:45211:ScruSet methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		self subclassResponsibility!
	*/

	/**
	 * Whether another currently has all my elements
	 */
	public boolean isSubsetOf(ScruSet another) {
		Stepper stepper = stepper();
		try {
			Heaper elem;
			while ((elem = (Heaper) stepper.fetch()) != null) {
				if (!(another.hasMember(elem))) {
					return false;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return true;
		/*
		udanax-top.st:45215:ScruSet methodsFor: 'testing'!
		{BooleanVar} isSubsetOf: another {ScruSet} 
			"Whether another currently has all my elements"
			self stepper forEach: 
				[:elem {Heaper} | 
				(another hasMember: elem) ifFalse: [^false]].
			^true!
		*/
	}

	/////////////////////////////////////////////
	// Creation

	/**
	 * A new one whose initial state is my current state, but that doesn't track
	 * changes. Note that there is no implication that these can be 'destroy'ed
	 * separately, because (for example) an ImmuSet just returns itself
	 */
	public abstract ScruSet copy();
	/*
	udanax-top.st:45224:ScruSet methodsFor: 'creation'!
	{ScruSet} copy
		"A new one whose initial state is my current state, but that doesn't track 
		changes. Note that there is no implication that these can be 'destroy'ed 
		separately, because (for example) an ImmuSet just returns itself"
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Conversion

	/**
	 * The elements in the set in an array, in some random order
	 */
	public PtrArray asArray() {
		//TODO thingToDo(); /* make this faster */
		PtrArray result = PtrArray.make(count().asInt32());
		Stepper mine = stepper();
		for (int index = 0; index < result.count(); index++) {
			result.store(index, mine.fetch());
			mine.step();
		}
		mine.destroy();
		return result;
		/*
		udanax-top.st:45233:ScruSet methodsFor: 'conversion'!
		{PtrArray} asArray
			"The elements in the set in an array, in some random order"
			
			| result {PtrArray} mine {Stepper} |
			self thingToDo. "make this faster"
			result := PtrArray nulls: self count DOTasLong.
			mine := self stepper.
			Int32Zero almostTo: result count do: [ :index {Int32} |
				result at: index store: mine fetch.
				mine step].
			mine destroy.
			^result!
		*/
	}

	/**
	 * Return an immu snapshot of my current state. Should probably be done with a
	 * Converter rather than with a message (for the reasons listed in the Converter
	 * class comment). In terms of the Stamp/Bert analogy mentioned in the class
	 * comment, asImmuSet is like asking for the current Stamp.
	 */
	public abstract ImmuSet asImmuSet();
	/*
	udanax-top.st:45246:ScruSet methodsFor: 'conversion'!
	{ImmuSet} asImmuSet
		"Return an immu snapshot of my current state. Should probably be done with a 
		Converter rather than with a message (for the reasons listed in the Converter 
		class comment). In terms of the Stamp/Bert analogy mentioned in the class 
		comment, asImmuSet is like asking for the current Stamp."
		self subclassResponsibility!
	*/

	/**
	 * Return a Mu whose initial state is the same as my current state, but which
	 * will now deviate independently of me. In terms of the Stamp/Bert analogy
	 * mentioned in the class comment, asMuSet is like asking for a new Bert starting
	 * on the current Stamp.
	 */
	public abstract MuSet asMuSet();
	/*
	udanax-top.st:45254:ScruSet methodsFor: 'conversion'!
	{MuSet} asMuSet
		"Return a Mu whose initial state is the same as my current state, but which 
		will now deviate independently of me. In terms of the Stamp/Bert analogy 
		mentioned in the class comment, asMuSet is like asking for a new Bert starting 
		on the current Stamp."
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Printing

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		printOnWithSimpleSyntax(oo, "{", ", ", "}");
		/*
		udanax-top.st:45264:ScruSet methodsFor: 'printing'!
		{void} printOn: oo {ostream reference} 
			oo << self getCategory name.
			self
				printOnWithSimpleSyntax: oo
				with: '{'
				with: ', '
				with: '}'!
		*/
	}

	public void printOnWithSimpleSyntax(PrintWriter oo, String open, String sep, String close) {
		printOnWithSyntax(oo, open, sep, close, false);
		/*
		udanax-top.st:45272:ScruSet methodsFor: 'printing'!
		{void} printOnWithSimpleSyntax: oo {ostream reference} 
			with: open {char star} 
			with: sep {char star} 
			with: close {char star} 
			
			self printOnWithSyntax: oo with: open with: sep with: close with: false!
		*/
	}

	/**
	 * For example, if we have the set '{a, b, c}' and we print it with
	 * 'p->printOnWithSyntax(oo,
	 */
	public void printOnWithSyntax(PrintWriter oo, String openMarker, String separator, String closeMarker, boolean fullPrint) {
		/* << */
		/* ,  */
		/* ;  */
		/* ,  */
		/* >> */
		/* );', we get '<<a; b; c>>'.  This is a convenient little hack
			for printing with all sorts of separators and brackets. */
		oo .print(openMarker);
		if (isEmpty()) {
			oo.print("nullSet");
		} else {
			boolean printMore = !fullPrint;
			int elemCount = 0;
			Stepper dSet = stepper();
			while (dSet.hasValue() && (printMore)) {
				oo.print(dSet.fetch());
				dSet.step();
				if (dSet.hasValue()) {
					oo.print(separator);
				}
				if (printMore && ((elemCount += 1) > 200)) {
					printMore = false;
				}
			}
			if (!printMore && (dSet.hasValue())) {
				oo.print("etc...");
			}
		}
		oo .print(closeMarker);
		/*
		udanax-top.st:45279:ScruSet methodsFor: 'printing'!
		{void} printOnWithSyntax: oo {ostream reference} 
			with: open {char star} 
			with: sep {char star} 
			with: close {char star}
			with: fullPrint {BooleanVar default: false}
			"For example, if we have the set '{a, b, c}' and we print it with 
			'p->printOnWithSyntax(oo, ""<<"", ""; "", "">>"");', we get '<<a; b; c>>'.  This is a convenient little hack
			for printing with all sorts of separators and brackets."
			
			| dSet {Stepper} elemCount {IntegerVar} printMore {BooleanVar} |
			printMore _ fullPrint not.
			elemCount _ IntegerVar0.
			oo << open.
			self isEmpty
				ifTrue: [oo << 'nullSet']
				ifFalse: 
					[dSet _ self stepper.
					[dSet hasValue and: [printMore]]
						whileTrue: 
							[oo << dSet fetch.
							dSet step.
							dSet hasValue ifTrue: [oo << sep].
							(printMore and: [(elemCount _ elemCount + 1) > 200])
								ifTrue: [printMore _ false]]].
			(printMore not and: [dSet hasValue])
				ifTrue: [oo << 'etc...'].
			oo << close!
		*/
	}

	/////////////////////////////////////////////
	// Enumerating

	/**
	 * How many elements are currently in the set.  Being a set, if the same element is put into
	 * the set twice,
	 * it is only in the set once.  'Same' above is according to 'isEqual'.
	 */
	public abstract IntegerValue count();
	/*
	udanax-top.st:45309:ScruSet methodsFor: 'enumerating'!
	{IntegerVar} count
		"How many elements are currently in the set.  Being a set, if the same element is put into the set twice,
		it is only in the set once.  'Same' above is according to 'isEqual'."
		self subclassResponsibility!
	*/

	/**
	 * Returns a stepper which will enumerate all the elements of the set in some unspecified
	 * order
	 */
	public abstract Stepper stepper();
	/*
	udanax-top.st:45314:ScruSet methodsFor: 'enumerating'!
	{Stepper} stepper
		"Returns a stepper which will enumerate all the elements of the set in some unspecified order"
		self subclassResponsibility!
	*/

	/**
	 * Iff I contain exactly one member, return it.  Otherwise BLAST.
	 * The idea for this message is taken from the THE function of ONTIC
	 * (reference McAllester)
	 */
	public Heaper theOne() {
		if (!count().isEqual(IntegerValue.one())) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
		}
		Stepper stepper = stepper();
		Heaper result = stepper.fetch();
		stepper.destroy();
		return result;
		/*
		udanax-top.st:45318:ScruSet methodsFor: 'enumerating'!
		{Heaper} theOne
			"Iff I contain exactly one member, return it.  Otherwise BLAST.
			The idea for this message is taken from the THE function of ONTIC
			(reference McAllester)"
			| stepper {Stepper} result {Heaper} |
			self count ~= 1 ifTrue:
				[ Heaper BLAST: #NotOneElement ].
			stepper _ self stepper.
			result _ stepper fetch.
			stepper destroy.
			^ result!
		*/
	}

	//public void inspect() {
	//return InspectorView.open(( if (Sensor.leftShiftDown()) {
	//Inspector.inspect(this);
	//}
	//else {
	//SetInspector.inspect(this);
	//}
	//));
	///*
	//udanax-top.st:45333:ScruSet methodsFor: 'private: smalltalk: private'!
	//{void} inspect
	//	^InspectorView open: (Sensor leftShiftDown ifTrue: [Inspector inspect: self] ifFalse: [SetInspector inspect: self])!
	//*/
	//}

	///**
	// * return all of my elements in an ordered collection for smalltalk MVC hacking
	// */
	//public void asOrderedCollection() {
	//OrderedCollection result;
	//Stepper stomp;
	//result = OrderedCollection.new(count());
	//stomp = stepper();
	//while (stomp.hasValue()) {
	//result.add(stomp.get());
	//stomp.step();
	//}
	//return result
	///* | result {SortedCollection} stomp {Stepper} |
	//	result _ SortedCollection new: self count.
	//	stomp _ self stepper.
	//	[stomp hasValue]
	//		whileTrue: [result add: stomp get.
	//			stomp step].
	//	^result asOrderedCollection */
	//;
	///*
	//udanax-top.st:45338:ScruSet methodsFor: 'smalltalk: conversion'!
	//asOrderedCollection
	//	"return all of my elements in an ordered collection for smalltalk MVC hacking"
	//	| result {OrderedCollection} stomp {Stepper} |
	//	result _ OrderedCollection new: self count.
	//	stomp _ self stepper.
	//	[stomp hasValue]
	//		whileTrue: [result add: stomp get.
	//			stomp step].
	//	^result
	//	"| result {SortedCollection} stomp {Stepper} |
	//	result _ SortedCollection new: self count.
	//	stomp _ self stepper.
	//	[stomp hasValue]
	//		whileTrue: [result add: stomp get.
	//			stomp step].
	//	^result asOrderedCollection"!
	//*/
	//}

	//public void dox(BlockClosure aBlock) {
	//for (Iterator iterator = stepper().forEach() ; iterator.hasNext() ; )aBlock;
	///*
	//udanax-top.st:45356:ScruSet methodsFor: 'smalltalk: conversion'!
	//{void} do: aBlock {BlockClosure of: Heaper}
	//	self stepper forEach: aBlock!
	//*/
	//}

//	public static void problems() {
//		throw new UnsupportedOperationException();
//		//TODO return signals((NOT_IN_SET);
//		/*
//		udanax-top.st:45369:ScruSet class methodsFor: 'exceptions: exceptions'!
//		problems.NotInSet
//			^self signals: #(NotInSet)!
//		*/
//	}
}
