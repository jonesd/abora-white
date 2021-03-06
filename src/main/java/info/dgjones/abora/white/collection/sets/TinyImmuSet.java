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
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * This is an efficient implementation of ImmuSets for one element sets.
 */
public class TinyImmuSet extends ImmuSet {
	protected Heaper elementInternal;
	/*
	udanax-top.st:45721:
	ImmuSet subclass: #TinyImmuSet
		instanceVariableNames: 'elementInternal {Heaper}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:45725:
	TinyImmuSet comment:
	'This is an efficient implementation of ImmuSets for zero and one element sets.'!
	*/
	/*
	udanax-top.st:45727:
	(TinyImmuSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:45812:
	TinyImmuSet class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:45815:
	(TinyImmuSet getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	/**
	 * Initialize a singleton immuset
	 */
	protected TinyImmuSet(Heaper only) {
		super();
		elementInternal = only;
		/*
		udanax-top.st:45732:TinyImmuSet methodsFor: 'protected: creation'!
		create: only {Heaper}
			"Initialize a singleton immuset"
			super create.
			elementInternal _ only!
		*/
	}

	protected TinyImmuSet(Rcvr receiver) {
		super(receiver);
		elementInternal = receiver.receiveHeaper();
		/*
		udanax-top.st:45803:TinyImmuSet methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			elementInternal _ receiver receiveHeaper.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static ImmuSet make(Heaper aHeaper) {
		return new TinyImmuSet(aHeaper);
		/*
		udanax-top.st:45820:TinyImmuSet class methodsFor: 'create'!
		{ImmuSet} make: aHeaper {Heaper}
			^ self create: aHeaper!
		*/
	}

	/////////////////////////////////////////////
	// Enumerating

	public IntegerValue count() {
		return IntegerValue.one();
		/*
		udanax-top.st:45739:TinyImmuSet methodsFor: 'enumerating'!
		{IntegerVar} count
			^ 1!
		*/
	}

	public Stepper stepper() {
		return Stepper.itemStepper(elementInternal);
		/*
		udanax-top.st:45742:TinyImmuSet methodsFor: 'enumerating'!
		{Stepper} stepper
			^Stepper itemStepper: elementInternal!
		*/
	}

	public Heaper theOne() {
		return elementInternal;
		/*
		udanax-top.st:45745:TinyImmuSet methodsFor: 'enumerating'!
		{Heaper} theOne
			^ elementInternal!
		*/
	}

	/////////////////////////////////////////////
	// Adding-Removing

	public ImmuSet with(Heaper anElement) {
		if (elementInternal.isEqual(anElement)) {
			return this;
		} else {
			MuSet nuSet;
			nuSet = MuSet.make(anElement);
			nuSet.introduce(elementInternal);
			return ImmuSet.make(nuSet);
		}
		/*
		udanax-top.st:45750:TinyImmuSet methodsFor: 'adding-removing'!
		{ImmuSet} with: anElement {Heaper} 
			(elementInternal isEqual: anElement)
				ifTrue: [^self]
				ifFalse: [| nuSet {MuSet} |
							nuSet _ MuSet make.Heaper: anElement.
							nuSet introduce: elementInternal.
							^ImmuSet make: nuSet]!
		*/
	}

	public ImmuSet without(Heaper anElement) {
		if (elementInternal.isEqual(anElement)) {
			return ImmuSet.make();
		}
		return this;
		/*
		udanax-top.st:45758:TinyImmuSet methodsFor: 'adding-removing'!
		{ImmuSet} without: anElement {Heaper}
			(elementInternal isEqual: anElement) ifTrue: [^ ImmuSet make].
			^ self!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public boolean hasMember(Heaper someone) {
		return elementInternal.isEqual(someone);
		/*
		udanax-top.st:45764:TinyImmuSet methodsFor: 'accessing'!
		{BooleanVar} hasMember: someone {Heaper}
			^ elementInternal isEqual: someone!
		*/
	}

	public boolean isEmpty() {
		return false;
		/*
		udanax-top.st:45767:TinyImmuSet methodsFor: 'accessing'!
		{BooleanVar} isEmpty
			^ false!
		*/
	}

	public boolean isSubsetOf(ScruSet another) {
		return another.hasMember(elementInternal);
		/*
		udanax-top.st:45770:TinyImmuSet methodsFor: 'accessing'!
		{BooleanVar} isSubsetOf: another {ScruSet}
			^ another hasMember: elementInternal!
		*/
	}

	/////////////////////////////////////////////
	// Operations

	public ImmuSet intersect(ScruSet another) {
		if (another.hasMember(elementInternal)) {
			return this;
		} else {
			return ImmuSet.make();
		}
		/*
		udanax-top.st:45775:TinyImmuSet methodsFor: 'operations'!
		{ImmuSet} intersect: another {ScruSet} 
			(another hasMember: elementInternal)
				ifTrue: [^ self]
				ifFalse: [^ ImmuSet make]!
		*/
	}

	public ImmuSet minus(ScruSet another) {
		if (another.hasMember(elementInternal)) {
			return ImmuSet.make();
		} else {
			return this;
		}
		/*
		udanax-top.st:45780:TinyImmuSet methodsFor: 'operations'!
		{ImmuSet} minus: another {ScruSet}
			(another hasMember: elementInternal) 
				ifTrue: [^ ImmuSet make]
				ifFalse: [^ self]!
		*/
	}

	public ImmuSet unionWith(ScruSet another) {
		if (another.isEmpty()) {
			return this;
		} else {
			if (another.hasMember(elementInternal)) {
				return another.asImmuSet();
			}
			if (another.count().isGT(IntegerValue.make(5))) {
				return another.asImmuSet().unionWith(this);
			}
			MuSet nuSet = MuSet.make(elementInternal);
			nuSet.storeAll(another);
			return ImmuSet.make(nuSet);
		}
		/*
		udanax-top.st:45785:TinyImmuSet methodsFor: 'operations'!
		{ImmuSet} unionWith: another {ScruSet}
			another isEmpty 
				ifTrue: [^ self]
				ifFalse:
					[| nuSet {MuSet} |
					(another hasMember: elementInternal) ifTrue: [ ^ another asImmuSet ].
					another count > 5 ifTrue: [^another asImmuSet unionWith: self].
					nuSet _ MuSet make.Heaper: elementInternal.
					nuSet storeAll: another.
					^ ImmuSet make: nuSet]!
		*/
	}

	/////////////////////////////////////////////
	// Conversion

	public MuSet asMuSet() {
		return MuSet.make(elementInternal);
		/*
		udanax-top.st:45798:TinyImmuSet methodsFor: 'conversion'!
		{MuSet} asMuSet
			^ MuSet make.Heaper: elementInternal!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(elementInternal);
		/*
		udanax-top.st:45807:TinyImmuSet methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: elementInternal.!
		*/
	}
}
