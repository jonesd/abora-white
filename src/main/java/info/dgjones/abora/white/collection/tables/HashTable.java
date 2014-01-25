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
package info.dgjones.abora.white.collection.tables;

import info.dgjones.abora.white.collection.steppers.TableStepper;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public abstract class HashTable extends MuTable {
	/*
	udanax-top.st:48454:
	MuTable subclass: #HashTable
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:48458:
	(HashTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:48532:
	HashTable class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:48535:
	(HashTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/**
	 * Associate value with key, whether or not
	 * there is a previous association.
	 */
	public abstract Heaper atStore(Position key, Heaper value);
	/*
	udanax-top.st:48463:HashTable methodsFor: 'accessing'!
	{Heaper} at: key {Position} store: value {Heaper} 
		"Associate value with key, whether or not
		 there is a previous association."
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:48469:HashTable methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		
		self subclassResponsibility!
	*/

	public abstract IntegerValue count();
	/*
	udanax-top.st:48473:HashTable methodsFor: 'accessing'!
	{IntegerVar} count
		self subclassResponsibility!
	*/

	public abstract XnRegion domain();
	/*
	udanax-top.st:48477:HashTable methodsFor: 'accessing'!
	{XnRegion} domain
		self subclassResponsibility.!
	*/

	public abstract Heaper fetch(Position key);
	/*
	udanax-top.st:48481:HashTable methodsFor: 'accessing'!
	{Heaper} fetch: key {Position} 
		self subclassResponsibility!
	*/

	public abstract ScruTable subTable(XnRegion reg);
	/*
	udanax-top.st:48485:HashTable methodsFor: 'accessing'!
	{ScruTable} subTable: reg {XnRegion} 
		self subclassResponsibility!
	*/

	/**
	 * Remove a key->value association from the table.
	 * Do not blast (or do anything else) if the key is not in my current domain.
	 */
	public abstract boolean wipe(Position anIdx);
	/*
	udanax-top.st:48489:HashTable methodsFor: 'accessing'!
	{BooleanVar} wipe: anIdx {Position}
		"Remove a key->value association from the table.
		 Do not blast (or do anything else) if the key is not in my current domain."
		self subclassResponsibility!
	*/

	public abstract boolean includesKey(Position aKey);
	/*
	udanax-top.st:48497:HashTable methodsFor: 'testing'!
	{BooleanVar} includesKey: aKey {Position}
		self subclassResponsibility!
	*/

	public abstract boolean isEmpty();
	/*
	udanax-top.st:48500:HashTable methodsFor: 'testing'!
	{BooleanVar} isEmpty
		self subclassResponsibility.!
	*/

	public abstract TableStepper stepper(OrderSpec order);
	/*
	udanax-top.st:48505:HashTable methodsFor: 'enumerating'!
	{TableStepper} stepper: order {OrderSpec default: NULL}
		self subclassResponsibility!
	*/

	public abstract Heaper theOne();
	/*
	udanax-top.st:48508:HashTable methodsFor: 'enumerating'!
	{Heaper} theOne
		self subclassResponsibility!
	*/

	public abstract XnRegion runAt(Position key);
	/*
	udanax-top.st:48513:HashTable methodsFor: 'runs'!
	{XnRegion} runAt: key {Position} 
		self subclassResponsibility!
	*/

	public abstract ScruTable copy();
	/*
	udanax-top.st:48519:HashTable methodsFor: 'creation'!
	{ScruTable} copy
		self subclassResponsibility!
	*/

	public abstract ScruTable emptySize(IntegerValue size);
	/*
	udanax-top.st:48522:HashTable methodsFor: 'creation'!
	{ScruTable} emptySize: size {IntegerVar}
		self subclassResponsibility!
	*/

	protected HashTable() {
		super();
		/*
		udanax-top.st:48528:HashTable methodsFor: 'protected: create'!
		create
			super create!
		*/
	}

	protected HashTable(Rcvr rcvr) {
		super(rcvr);
	}

	public static MuTable make(CoordinateSpace cs) {
		return ActualHashTable.make(cs);
		/*
		udanax-top.st:48540:HashTable class methodsFor: 'pseudo constructors'!
		{HashTable INLINE} make.CoordinateSpace: cs {CoordinateSpace}
			^ActualHashTable make: cs!
		*/
	}

	public static HashTable make(CoordinateSpace cs, IntegerValue size) {
		//TODO suspicious asInt32
		return ActualHashTable.make(cs, IntegerValue.make(size.asInt32() | 1));
		/*
		udanax-top.st:48543:HashTable class methodsFor: 'pseudo constructors'!
		make.CoordinateSpace: cs {CoordinateSpace} with: size {IntegerVar}
			^ActualHashTable make: cs with: (size DOTasLong bitOr: 1)!
		*/
	}

	//	public static void initTimeNonInherited() {
	//		REQUIRES(ImmuSet.getCategory());
	//		/* for the empty set domain */
	//		REQUIRES(LPPrimeSizeProvider.getCategory());
	//		/*
	//		udanax-top.st:48548:HashTable class methodsFor: 'smalltalk: initialization'!
	//		initTimeNonInherited
	//			self REQUIRES: ImmuSet. "for the empty set domain"
	//			self REQUIRES: LPPrimeSizeProvider!
	//		*/
	//	}
}
