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
package org.abora.white.collection.steppers;

import org.abora.white.collection.tables.MuArray;
import org.abora.white.spaces.integers.IntegerRegion;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class ArrayAccumulator extends TableAccumulator {
	protected MuArray arrayInternal;
	/*
	udanax-top.st:12425:
	TableAccumulator subclass: #ArrayAccumulator
		instanceVariableNames: 'arrayInternal {MuArray}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:12429:
	(ArrayAccumulator getOrMakeCxxClassDescription)
		friends:
	'friend class XuArray;';
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:12456:
	ArrayAccumulator class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12459:
	(ArrayAccumulator getOrMakeCxxClassDescription)
		friends:
	'friend class XuArray;';
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected ArrayAccumulator(MuArray onTable) {
		super();
		arrayInternal = onTable;
		/*
		udanax-top.st:12436:ArrayAccumulator methodsFor: 'protected: create'!
		create: onTable {MuArray}
			super create.
			arrayInternal _ onTable!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static TableAccumulator make(MuArray onTable) {
		return new ArrayAccumulator(onTable);
		/*
		udanax-top.st:12466:ArrayAccumulator class methodsFor: 'create'!
		{TableAccumulator} make: onTable {MuArray}
			^ self create: onTable!
		*/
	}

	/////////////////////////////////////////////
	// Operations

	public void step(Heaper obj) {
		if (arrayInternal.isEmpty()) {
			arrayInternal.atIntStore(IntegerValue.zero(), obj);
		} else {
			arrayInternal.atIntIntroduce(((IntegerRegion) arrayInternal.domain()).stop(), obj);
		}
		/*
		udanax-top.st:12442:ArrayAccumulator methodsFor: 'operations'!
		{void} step: obj {Heaper} 
			arrayInternal isEmpty
				ifTrue: [arrayInternal atInt: IntegerVar0 store: obj]
				ifFalse: [arrayInternal atInt: (arrayInternal domain quickCast: IntegerRegion) stop introduce: obj]!
		*/
	}

	public Heaper value() {
		return arrayInternal;
		/*
		udanax-top.st:12447:ArrayAccumulator methodsFor: 'operations'!
		{Heaper} value
			^ arrayInternal.!
		*/
	}

	/////////////////////////////////////////////
	// Create

	public Accumulator copy() {
		return ArrayAccumulator.make(((MuArray) arrayInternal.copy()));
		/*
		udanax-top.st:12452:ArrayAccumulator methodsFor: 'create'!
		{Accumulator} copy
			^ ArrayAccumulator make: (arrayInternal copy cast: MuArray)!
		*/
	}


//	public static void create(Object aTable) {
//		return new ArrayAccumulator(aTable);
//		/*
//		udanax-top.st:12471:ArrayAccumulator class methodsFor: 'smalltalk: creation'!
//		create.IntegerTable: aTable	
//			^self new create: aTable!
//		*/
//	}
}
