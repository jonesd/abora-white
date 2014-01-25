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

import info.dgjones.abora.white.collection.tables.OberIntegerTable;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class ITAscendingStepper extends IntegerTableStepper {
	protected OberIntegerTable arrayInternal;
	protected int indexInternal;
	protected int lastValueInternal;
	/*
	udanax-top.st:55869:
	IntegerTableStepper subclass: #ITAscendingStepper
		instanceVariableNames: '
			arrayInternal {OberIntegerTable}
			indexInternal {UInt32}
			lastValueInternal {UInt32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:55876:
	(ITAscendingStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		if (indexInternal <= lastValueInternal) {
			return arrayInternal.elementsArray().fetch(indexInternal);
		} else {
			return null;
		}
		/*
		udanax-top.st:55881:ITAscendingStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			(indexInternal <= lastValueInternal)
				ifTrue: [^arrayInternal elementsArray fetch: indexInternal]
				ifFalse: [^NULL]!
		*/
	}

	public boolean hasValue() {
		return indexInternal <= lastValueInternal;
		/*
		udanax-top.st:55886:ITAscendingStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^ indexInternal <= lastValueInternal!
		*/
	}

	public void step() {
		indexInternal = indexInternal + 1;
		while (indexInternal <= lastValueInternal && ((arrayInternal.elementsArray().fetch(indexInternal)) == null)) {
			indexInternal = indexInternal + 1;
		}
		/*
		udanax-top.st:55889:ITAscendingStepper methodsFor: 'operations'!
		{void} step
			indexInternal _ indexInternal + 1.
			[indexInternal <= lastValueInternal and: 
					[(arrayInternal elementsArray fetch: indexInternal) == NULL]]
				whileTrue: [indexInternal _ indexInternal + 1]!
		*/
	}

	public Stepper copy() {
		return new ITAscendingStepper(((OberIntegerTable) arrayInternal.copy()), index(), arrayInternal.startIndex().plus(IntegerValue.make(lastValueInternal)));
		/*
		udanax-top.st:55897:ITAscendingStepper methodsFor: 'create'!
		{Stepper} copy
			^ITAscendingStepper
				create: (arrayInternal copy cast: OberIntegerTable)
				with: self index
				with: arrayInternal startIndex + lastValueInternal!
		*/
	}

	public ITAscendingStepper(OberIntegerTable array) {
		super();
		arrayInternal = ((OberIntegerTable) array.copy());
		indexInternal = arrayInternal.startOffset();
		lastValueInternal = arrayInternal.endOffset();
		verifyEntry();
		/*
		udanax-top.st:55903:ITAscendingStepper methodsFor: 'create'!
		create: array {OberIntegerTable}
			super create.
			arrayInternal _ (array copy cast: OberIntegerTable).
			indexInternal _ arrayInternal startOffset.
			lastValueInternal _ arrayInternal endOffset.
			self verifyEntry!
		*/
	}

	public ITAscendingStepper(OberIntegerTable array, IntegerValue index) {
		super();
		arrayInternal = ((OberIntegerTable) array.copy());
		indexInternal = (index.minus(arrayInternal.startIndex())).asInt32();
		lastValueInternal = arrayInternal.endOffset();
		verifyEntry();
		/*
		udanax-top.st:55910:ITAscendingStepper methodsFor: 'create'!
		create: array {OberIntegerTable} with: index {IntegerVar}
			super create.
			arrayInternal _ (array copy cast: OberIntegerTable).
			indexInternal _ (index - arrayInternal startIndex) DOTasLong.
			lastValueInternal _ arrayInternal endOffset.
			self verifyEntry!
		*/
	}

	/**
	 * n.b. !!!!!!!! This constructor DOES NOT COPY the table because this
	 * constructor is used by the table copy (which creates a stepper).
	 * The copy is done in the table->stepper(NULL) routine before calling
	 * this constructor.
	 */
	public ITAscendingStepper(OberIntegerTable array, IntegerValue start, IntegerValue stop) {
		super();
		arrayInternal = array;
		indexInternal = (start.minus(arrayInternal.startIndex())).asInt32();
		lastValueInternal = (stop.minus(arrayInternal.startIndex())).asInt32();
		verifyEntry();
		/*
		udanax-top.st:55917:ITAscendingStepper methodsFor: 'create'!
		create: array {OberIntegerTable} with: start {IntegerVar} with: stop {IntegerVar} 
			"n.b. !!!!!!!! This constructor DOES NOT COPY the table because this 
			constructor is used by the table copy (which creates a stepper). 
			The copy is done in the table->stepper(NULL) routine before calling 
			this constructor."
			super create.
			arrayInternal _ array.
			indexInternal _ (start - arrayInternal startIndex) DOTasLong.
			lastValueInternal _ (stop - arrayInternal startIndex) DOTasLong.
			self verifyEntry!
		*/
	}

	public IntegerValue index() {
		return arrayInternal.startIndex().plus(IntegerValue.make(indexInternal));
		/*
		udanax-top.st:55931:ITAscendingStepper methodsFor: 'special'!
		{IntegerVar} index
			^arrayInternal startIndex + indexInternal!
		*/
	}

	public Position position() {
		return index().integer();
		/*
		udanax-top.st:55934:ITAscendingStepper methodsFor: 'special'!
		{Position} position
			^ self index integer!
		*/
	}

	public void verifyEntry() {
		while (indexInternal <= lastValueInternal && ((arrayInternal.elementsArray().fetch(indexInternal)) == null)) {
			indexInternal = indexInternal + 1;
		}
		/*
		udanax-top.st:55939:ITAscendingStepper methodsFor: 'private: private'!
		{void} verifyEntry
			[indexInternal <= lastValueInternal and: 
					[(arrayInternal elementsArray fetch: indexInternal) == NULL]]
				whileTrue: [indexInternal _ indexInternal + 1]!
		*/
	}
}
