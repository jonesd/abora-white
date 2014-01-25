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
package info.dgjones.abora.white.collection.steppers;

import info.dgjones.abora.white.collection.tables.MuArray;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

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
