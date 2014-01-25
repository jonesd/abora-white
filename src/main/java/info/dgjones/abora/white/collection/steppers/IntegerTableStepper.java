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

import info.dgjones.abora.white.collection.tables.ActualIntegerTable;
import info.dgjones.abora.white.collection.tables.IntegerTable;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Consider this a protected class.  It is public only for use by the "array" module.
 */
public abstract class IntegerTableStepper extends TableStepper {
	/*
	udanax-top.st:55790:
	TableStepper subclass: #IntegerTableStepper
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:55794:
	IntegerTableStepper comment:
	'Consider this a protected class.  It is public only for use by the "array" module.'!
	*/
	/*
	udanax-top.st:55796:
	(IntegerTableStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:55827:
	IntegerTableStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:55830:
	(IntegerTableStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	public abstract Heaper fetch();
	/*
	udanax-top.st:55801:IntegerTableStepper methodsFor: 'operations'!
	{Heaper wimpy} fetch
		self subclassResponsibility!
	*/

	public Heaper get() {
		Heaper res;
		res = fetch();
		if (res == null) {
			throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
		}
		return res;
		/*
		udanax-top.st:55804:IntegerTableStepper methodsFor: 'operations'!
		{Heaper wimpy} get
			| res {Heaper wimpy} |
			res _ self fetch.
			res == NULL ifTrue: [Heaper BLAST: #EmptyStepper].
			^res!
		*/
	}

	public abstract boolean hasValue();
	/*
	udanax-top.st:55810:IntegerTableStepper methodsFor: 'operations'!
	{BooleanVar} hasValue
		self subclassResponsibility!
	*/

	public abstract void step();
	/*
	udanax-top.st:55813:IntegerTableStepper methodsFor: 'operations'!
	{void} step
		self subclassResponsibility!
	*/

	public abstract Position position();
	/*
	udanax-top.st:55818:IntegerTableStepper methodsFor: 'special'!
	{Position} position
		self subclassResponsibility!
	*/

	public abstract Stepper copy();
	/*
	udanax-top.st:55823:IntegerTableStepper methodsFor: 'create'!
	{Stepper} copy
		self subclassResponsibility!
	*/

	/**
	 * Do not consider public.  Only for use by the modules inttab, array, and awarray.
	 */
	public static TableStepper make(IntegerTable aTable, OrderSpec anOrder) {
		if (aTable instanceof ActualIntegerTable) {
			ActualIntegerTable tab = (ActualIntegerTable) aTable;
			if (anOrder.followsInt(IntegerValue.one(), IntegerValue.zero())) {
				return new ITAscendingStepper(tab);
			} else {
				return new ITDescendingStepper(tab);
			}
		} else {
			if (anOrder == null) {
				return new ITGenericStepper(aTable);
			} else {
				return new ITGenericStepper(aTable, anOrder);
			}
		}
		/*
		udanax-top.st:55835:IntegerTableStepper class methodsFor: 'pseudoConstructors'!
		make: aTable {IntegerTable} with: anOrder {OrderSpec default: NULL} 
			"Do not consider public.  Only for use by the modules inttab, array, and awarray."
			
			aTable cast: ActualIntegerTable into: [:tab |
					(anOrder followsInt: 1  with: IntegerVar0)
						ifTrue: [^ITAscendingStepper create: tab]
						ifFalse: [^ITDescendingStepper create: tab]]
				others: [anOrder == NULL
						ifTrue: [^ITGenericStepper create: aTable]
						ifFalse: [^ITGenericStepper create: aTable with.OrderSpec: anOrder]].
			^ NULL "compiler fodder"!
		*/
	}

	/**
	 * Do not consider public.  Only for use by the modules inttab, array, and awarray.
	 */
	public static TableStepper make(IntegerTable aTable, IntegerValue start, IntegerValue stop) {
		if (aTable instanceof ActualIntegerTable) {
			ActualIntegerTable tab = (ActualIntegerTable) aTable;
			return new ITAscendingStepper(tab, start, stop);
		} else {
			return new ITGenericStepper(aTable, start, stop, IntegerValue.one());
		}
		/*
		udanax-top.st:55847:IntegerTableStepper class methodsFor: 'pseudoConstructors'!
		make: aTable {IntegerTable} 
			with: start {IntegerVar} 
			with: stop {IntegerVar} 
			"Do not consider public.  Only for use by the modules inttab, array, and awarray."
			
			aTable cast: ActualIntegerTable into: [:tab |
					^ITAscendingStepper
						create: tab
						with: start
						with: stop]
				others: [^ITGenericStepper
						create: aTable
						with: start
						with: stop
						with: 1].
			^ NULL "compiler fodder"!
		*/
	}

//	public static TableStepper create(IntegerTable onTable, OrderSpec anOrderSpec) {
//		return new IntegerTableStepper(onTable, anOrderSpec);
//		/*
//		udanax-top.st:55866:IntegerTableStepper class methodsFor: 'smalltalk: smalltalk creation'!
//		create: onTable {IntegerTable} with.OrderSpec: anOrderSpec {OrderSpec}
//			^ self new create: onTable with.OrderSpec: anOrderSpec!
//		*/
//	}
}
