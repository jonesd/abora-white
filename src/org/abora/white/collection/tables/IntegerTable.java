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
package org.abora.white.collection.tables;

import org.abora.white.collection.steppers.TableStepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.OrderSpec;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.spaces.integers.IntegerPos;
import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.spaces.integers.IntegerSpace;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * The IntegerTable class is used for tables that have arbitrary XuInteger keys in their
 * domain.  Since ScruTable & MuTable already provide all the unboxed versions of the table
 * protocol, there is little need for this class to be a type.  However, this class does
 * provide a bit of extra protocol convenience: highestIndex & lowestIndex. Unless these are
 * retired, we cannot retire this class from type status.
 * <p>
 * Note that there may be tables with XuInteger keys (i.e., IntegerSpace domains) which are
 * not kinds of IntegerTables.  In particular it is perfectly sensible to create a HashTable
 * with XuInteger keys when the domain region is likely to be sparse.
 */
public abstract class IntegerTable extends MuTable {
	/*
	udanax-top.st:48897:
	MuTable subclass: #IntegerTable
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:48901:
	IntegerTable comment:
	'The IntegerTable class is used for tables that have arbitrary XuInteger keys in their domain.  Since ScruTable & MuTable already provide all the unboxed versions of the table protocol, there is little need for this class to be a type.  However, this class does provide a bit of extra protocol convenience: highestIndex & lowestIndex. Unless these are retired, we cannot retire this class from type status.
		
		Note that there may be tables with XuInteger keys (i.e., IntegerSpace domains) which are not kinds of IntegerTables.  In particular it is perfectly sensible to create a HashTable with XuInteger keys when the domain region is likely to be sparse.'!
	*/
	/*
	udanax-top.st:48905:
	(IntegerTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:49045:
	IntegerTable class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:49048:
	(IntegerTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	public void atIntIntroduce(IntegerValue key, Heaper value) {
		Heaper old;
		if ((old = atIntStore(key, value)) != null) {
			atIntStore(key, old);
			/* restore prior condition */
			throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_TABLE);
		}
		/*
		udanax-top.st:48910:IntegerTable methodsFor: 'accessing'!
		{void} atInt: key {IntegerVar} introduce: value {Heaper}
			| old {Heaper} |
			(old _ self atInt: key store: value) ~~ NULL
				ifTrue: [self atInt: key store: old. "restore prior condition"
					Heaper BLAST: #AlreadyInTable]!
		*/
	}

	public void atIntReplace(IntegerValue key, Heaper value) {
		if (atIntStore(key, value) == null) {
			intWipe(key);
			/* restore prior condition */
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
		}
		/*
		udanax-top.st:48916:IntegerTable methodsFor: 'accessing'!
		{void} atInt: key {IntegerVar} replace: value {Heaper} 
			(self atInt: key store: value) == NULL
				ifTrue: [self intWipe: key. "restore prior condition"
					Heaper BLAST: #NotInTable]!
		*/
	}

	public abstract Heaper atIntStore(IntegerValue key, Heaper value);
	/*
	udanax-top.st:48921:IntegerTable methodsFor: 'accessing'!
	{Heaper} atInt: key {IntegerVar} store: value {Heaper} 
		self subclassResponsibility!
	*/

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:48925:IntegerTable methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^IntegerSpace make!
		*/
	}

	public abstract IntegerValue count();
	/*
	udanax-top.st:48929:IntegerTable methodsFor: 'accessing'!
	{IntegerVar} count
		self subclassResponsibility!
	*/

	public abstract XnRegion domain();
	/*
	udanax-top.st:48933:IntegerTable methodsFor: 'accessing'!
	{XnRegion} domain
		self subclassResponsibility.!
	*/

	/**
	 * Given that the table is non-empty, 'intTab->highestIndex()' is equivalent to
	 * 'CAST(IntegerRegion,intTab->domain())->upperBound() -1'. The reason for the
	 * '-1' is that the 'upperBound' is an exclusive upper bound (see
	 * IntegerRegion::upperBound), whereas 'highestIndex' is the highest index which is
	 * in my domain. I need to here specify what 'highestIndex' does if I am empty.
	 */
	public abstract IntegerValue highestIndex();
	/*
	udanax-top.st:48937:IntegerTable methodsFor: 'accessing'!
	{IntegerVar} highestIndex
		"Given that the table is non-empty, 'intTab->highestIndex()' is equivalent to 
		'CAST(IntegerRegion,intTab->domain())->upperBound() -1'. The reason for the 
		'-1' is that the 'upperBound' is an exclusive upper bound (see 
		IntegerRegion::upperBound), whereas 'highestIndex' is the highest index which is 
		in my domain. I need to here specify what 'highestIndex' does if I am empty."
		self subclassResponsibility!
	*/

	public abstract Heaper intFetch(IntegerValue key);
	/*
	udanax-top.st:48946:IntegerTable methodsFor: 'accessing'!
	{Heaper} intFetch: key {IntegerVar} 
		self subclassResponsibility!
	*/

	public void intRemove(IntegerValue anIdx) {
		if (!(intWipe(anIdx))) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_TABLE);
		}
		/*
		udanax-top.st:48950:IntegerTable methodsFor: 'accessing'!
		{void} intRemove: anIdx {IntegerVar}
			
			(self intWipe: anIdx) ifFalse: [Heaper BLAST: #NotInTable]!
		*/
	}

	public abstract boolean intWipe(IntegerValue anIdx);
	/*
	udanax-top.st:48954:IntegerTable methodsFor: 'accessing'!
	{BooleanVar} intWipe: anIdx {IntegerVar}
		self subclassResponsibility!
	*/

	/**
	 * Given that the table is non-empty, 'intTab->lowestIndex()' is equivalent to
	 * 'CAST(IntegerRegion,intTab->domain())->lowerBound()'. 'lowestIndex' is the
	 * lowest index which is in my domain. I need to here specify what 'lowestIndex'
	 * does if I am empty.
	 */
	public abstract IntegerValue lowestIndex();
	/*
	udanax-top.st:48957:IntegerTable methodsFor: 'accessing'!
	{IntegerVar} lowestIndex
		"Given that the table is non-empty, 'intTab->lowestIndex()' is equivalent to 
		'CAST(IntegerRegion,intTab->domain())->lowerBound()'. 'lowestIndex' is the 
		lowest index which is in my domain. I need to here specify what 'lowestIndex' 
		does if I am empty."
		self subclassResponsibility!
	*/

	public abstract ScruTable subTable(XnRegion reg);
	/*
	udanax-top.st:48965:IntegerTable methodsFor: 'accessing'!
	{ScruTable} subTable: reg {XnRegion} 
		self subclassResponsibility!
	*/

	public void atIntroduce(Position key, Heaper value) {
		atIntIntroduce(((IntegerPos) key).asIntegerVar(), value);
		/*
		udanax-top.st:48971:IntegerTable methodsFor: 'accessing overloads'!
		{void} at: key {Position} introduce: value {Heaper} 
			self atInt: (key cast: IntegerPos) asIntegerVar introduce: value!
		*/
	}

	public void atReplace(Position key, Heaper value) {
		atIntReplace(((IntegerPos) key).asIntegerVar(), value);
		/*
		udanax-top.st:48975:IntegerTable methodsFor: 'accessing overloads'!
		{void} at: key {Position} replace: value {Heaper} 
			self atInt: (key cast: IntegerPos) asIntegerVar replace: value!
		*/
	}

	public Heaper atStore(Position key, Heaper value) {
		return atIntStore(((IntegerPos) key).asIntegerVar(), value);
		/*
		udanax-top.st:48979:IntegerTable methodsFor: 'accessing overloads'!
		{Heaper} at: key {Position} store: value {Heaper} 
			^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
		*/
	}

	public Heaper fetch(Position key) {
		return intFetch(((IntegerPos) key).asIntegerVar());
		/*
		udanax-top.st:48983:IntegerTable methodsFor: 'accessing overloads'!
		{Heaper} fetch: key {Position} 
			^ self intFetch: (key cast: IntegerPos) asIntegerVar!
		*/
	}

	public boolean includesKey(Position aKey) {
		return includesIntKey(((IntegerPos) aKey).asIntegerVar());
		/*
		udanax-top.st:48987:IntegerTable methodsFor: 'accessing overloads'!
		{BooleanVar} includesKey: aKey {Position}
			^ self includesIntKey: (aKey cast: IntegerPos) asIntegerVar!
		*/
	}

	public void remove(Position aPos) {
		intRemove(((IntegerPos) aPos).asIntegerVar());
		/*
		udanax-top.st:48990:IntegerTable methodsFor: 'accessing overloads'!
		{void} remove: aPos {Position}
			
			self intRemove: (aPos cast: IntegerPos) asIntegerVar!
		*/
	}

	public XnRegion runAt(Position key) {
		return runAtInt(((IntegerPos) key).asIntegerVar());
		/*
		udanax-top.st:48994:IntegerTable methodsFor: 'accessing overloads'!
		{XnRegion} runAt: key {Position} 
			^ self runAtInt: (key cast: IntegerPos) asIntegerVar!
		*/
	}

	public boolean wipe(Position anIdx) {
		return intWipe((((IntegerPos) anIdx).asIntegerVar()));
		/*
		udanax-top.st:48997:IntegerTable methodsFor: 'accessing overloads'!
		{BooleanVar} wipe: anIdx {Position}
			^ self intWipe: ((anIdx cast: IntegerPos) asIntegerVar)!
		*/
	}

	public abstract boolean includesIntKey(IntegerValue aKey);
	/*
	udanax-top.st:49003:IntegerTable methodsFor: 'testing'!
	{BooleanVar} includesIntKey: aKey {IntegerVar}
		self subclassResponsibility!
	*/

	public abstract boolean isEmpty();
	/*
	udanax-top.st:49006:IntegerTable methodsFor: 'testing'!
	{BooleanVar} isEmpty
		self subclassResponsibility.!
	*/

	public abstract TableStepper stepper(OrderSpec order);
	/*
	udanax-top.st:49011:IntegerTable methodsFor: 'enumerating'!
	{TableStepper} stepper: order {OrderSpec default: NULL}
		self subclassResponsibility!
	*/

	public abstract XnRegion runAtInt(IntegerValue key);
	/*
	udanax-top.st:49016:IntegerTable methodsFor: 'runs'!
	{XnRegion} runAtInt: key {IntegerVar}
		self subclassResponsibility!
	*/

	public abstract ScruTable copy();
	/*
	udanax-top.st:49021:IntegerTable methodsFor: 'creation'!
	{ScruTable} copy
		self subclassResponsibility!
	*/

	/**
	 * Create a new table with an unspecified number of initial domain positions.
	 */
	protected IntegerTable() {
		super();
		/*
		udanax-top.st:49024:IntegerTable methodsFor: 'creation'!
		create
			"Create a new table with an unspecified number of initial domain positions."
			super create!
		*/
	}

	protected IntegerTable(Rcvr rcvr) {
		super(rcvr);
	}

	public abstract ScruTable emptySize(IntegerValue size);
	/*
	udanax-top.st:49028:IntegerTable methodsFor: 'creation'!
	{ScruTable} emptySize: size {IntegerVar}
		self subclassResponsibility!
	*/

	/**
	 * Return a table which contains the elements from start to stop, starting at
	 * firstIndex. Zero-based subclasses will blast if firstIndex is non-zero
	 */
	public abstract ScruTable offsetSubTableBetween(IntegerValue startIndex, IntegerValue stopIndex, IntegerValue firstIndex);
	/*
	udanax-top.st:49032:IntegerTable methodsFor: 'creation'!
	{ScruTable} offsetSubTableBetween: startIndex {IntegerVar} 
		with: stopIndex {IntegerVar} 
		with: firstIndex {IntegerVar} 
		"Return a table which contains the elements from start to stop, starting at 
		firstIndex. Zero-based subclasses will blast if firstIndex is non-zero"
		self subclassResponsibility!
	*/

	/**
	 * Hack for C++ overloading problem
	 */
	public abstract ScruTable subTableBetween(IntegerValue startIndex, IntegerValue stopIndex);
	/*
	udanax-top.st:49040:IntegerTable methodsFor: 'creation'!
	{ScruTable} subTableBetween: startIndex {IntegerVar} with: stopIndex {IntegerVar}
		"Hack for C++ overloading problem"
		self subclassResponsibility!
	*/

//	public static Heaper make(Heaper something) {
//		if (something instanceof String) {
//			return make(something);
//		}
//		return make(((IntegerValue) something));
//		/*
//		udanax-top.st:49053:IntegerTable class methodsFor: 'smalltalk: pseudoConstructors'!
//		{IntegerTable} make: something {Heaper}
//			(something isKindOf: String) 
//				ifTrue: [^ self make.charVector: something].
//			^ self make.IntegerVar: (something cast: Integer)!
//		*/
//	}

	//	public static Heaper make(IntegerValue from, IntegerValue to) {
	//		return make(from, to);
	//		/*
	//		udanax-top.st:49058:IntegerTable class methodsFor: 'smalltalk: pseudoConstructors'!
	//		{MuTable} make: from {IntegerVar} with: to {IntegerVar}
	//			^self make.IntegerVar: from with: to!
	//		*/
	//	}

	/**
	 * A new empty IntegerTable
	 */
	public static IntegerTable make() {
		return new ActualIntegerTable();
		/*
		udanax-top.st:49064:IntegerTable class methodsFor: 'pseudoConstructors'!
		{IntegerTable} make
			"A new empty IntegerTable"
			
			^ ActualIntegerTable create.!
		*/
	}

	/**
	 * A new empty IntegerTable. 'someSize' is a hint about how big the table is likely
	 * to need to be ('highestIndex - lowestIndex + 1', not 'count').
	 */
	public static IntegerTable make(IntegerValue someSize) {
		return new ActualIntegerTable(someSize);
		/*
		udanax-top.st:49069:IntegerTable class methodsFor: 'pseudoConstructors'!
		{IntegerTable} make.IntegerVar: someSize {IntegerVar} 
			"A new empty IntegerTable. 'someSize' is a hint about how big the table is likely 
			to need to be ('highestIndex - lowestIndex + 1', not 'count')."
			^ActualIntegerTable create.IntegerVar: someSize!
		*/
	}

	/**
	 * Hint that the domain's lowerBound (inclusive) will eventually be 'fromIdx', and
	 * the domain's upperBound (exclusive) will eventually be 'toIdx'.
	 */
	public static IntegerTable make(IntegerValue fromIdx, IntegerValue toIdx) {
		return new ActualIntegerTable(fromIdx, toIdx);
		/*
		udanax-top.st:49075:IntegerTable class methodsFor: 'pseudoConstructors'!
		{IntegerTable} make.IntegerVar: fromIdx {IntegerVar} with: toIdx {IntegerVar} 
			"Hint that the domain's lowerBound (inclusive) will eventually be 'fromIdx', and 
			the domain's upperBound (exclusive) will eventually be 'toIdx'."
			^ActualIntegerTable create: fromIdx with: toIdx!
		*/
	}

	/**
	 * Hint that the domain of the new table will eventually be (or at least resemble)
	 * 'reg'.
	 */
	public static IntegerTable make(IntegerRegion reg) {
		return new ActualIntegerTable(reg.start(), reg.stop());
		/*
		udanax-top.st:49081:IntegerTable class methodsFor: 'pseudoConstructors'!
		{IntegerTable} make.Region: reg {IntegerRegion} 
			"Hint that the domain of the new table will eventually be (or at least resemble) 
			'reg'."
			^ActualIntegerTable create: reg start with: reg stop!
		*/
	}

	//	/**
	//	 * A new IntegerTable initialized from 'table' in a wierd and screwy way
	//	 */
	//	public static Heaper make(ScruTable table) {
	//		/* | newTable {IntegerTable} stomp {TableStepper} |
	//			newTable _ IntegerTable make.IntegerVar: (table count).
	//			stomp _ table stepper.
	//			[stomp hasValue] whileTrue:
	//				[newTable at.IntegerVar: stomp index introduce: (table get.IntegerVar: stomp index).
	//				stomp step].
	//			^newTable */
	//		passe();
	//		/*
	//		udanax-top.st:49089:IntegerTable class methodsFor: 'smalltalk: passe'!
	//		{IntegerTable} make.ScruTable: table {ScruTable} 
	//			"A new IntegerTable initialized from 'table' in a wierd and screwy way"
	//			
	//			"| newTable {IntegerTable} stomp {TableStepper} |
	//			newTable _ IntegerTable make.IntegerVar: (table count).
	//			stomp _ table stepper.
	//			[stomp hasValue] whileTrue:
	//				[newTable at.IntegerVar: stomp index introduce: (table get.IntegerVar: stomp index).
	//				stomp step].
	//			^newTable"
	//			
	//			self passe!
	//		*/
	//	}

	//	/**
	//	 * Make a copy of 'wv' as an IntegerTable. The IntegerTable starts out with the
	//	 * same state as 'wv', but unlike 'wv' is not obligated to maintain MuArray
	//	 * constraints.
	//	 */
	//	public static Heaper make(MuWordArray wv) {
	//		passe();
	//		/*
	//		udanax-top.st:49102:IntegerTable class methodsFor: 'smalltalk: passe'!
	//		{IntegerTable} make.WordArray: wv {MuWordArray}
	//			"Make a copy of 'wv' as an IntegerTable. The IntegerTable starts out with the 
	//			same state as 'wv', but unlike 'wv' is not obligated to maintain MuArray 
	//			constraints."
	//			self passe.!
	//		*/
	//	}
}
