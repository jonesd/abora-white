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
package info.dgjones.abora.white.tabtool;

import info.dgjones.abora.white.collection.arrays.Int32Array;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * This is a non-stepper stepper that returns a stream of prime numbers.
 * SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of
 * increased table size, LPPrimeSizeProvider does not claim to do this.
 * - michael 31 July 1991
 */
public class PrimeSizeProvider extends Heaper {
	protected Int32Array smallPrimeTable;
	/*
	udanax-top.st:33086:
	Heaper subclass: #PrimeSizeProvider
		instanceVariableNames: 'smallPrimeTable {UInt32Array}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tabtool'!
	*/
	/*
	udanax-top.st:33090:
	PrimeSizeProvider comment:
	'This is a non-stepper stepper that returns a stream of prime numbers.
	SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of increased table size, LPPrimeSizeProvider does not claim to do this.
	 - michael 31 July 1991'!
	*/
	/*
	udanax-top.st:33096:
	(PrimeSizeProvider getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/
	/*
	udanax-top.st:33140:
	PrimeSizeProvider class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:33143:
	(PrimeSizeProvider getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected PrimeSizeProvider(Int32Array aSmallPrimeTable) {
		super();
		smallPrimeTable = aSmallPrimeTable;
		/*
		udanax-top.st:33129:PrimeSizeProvider methodsFor: 'creation'!
		create: aSmallPrimeTable {UInt32Array}
			super create.
			smallPrimeTable _ aSmallPrimeTable.!
		*/
	}


	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static PrimeSizeProvider make() {
		return LPPrimeSizeProvider.make();
		/*
		udanax-top.st:33148:PrimeSizeProvider class methodsFor: 'creation'!
		{PrimeSizeProvider INLINE} make
			^LPPrimeSizeProvider make!
		*/
	}


	/////////////////////////////////////////////
	// Accessing

	public IntegerValue primeAfter(IntegerValue attempt) {
		int idx = 0;
		int val = attempt.asInt32();
		//TODO what if attempt > Int32.MAX?
		int lim = smallPrimeTable.count();
		while ((idx < lim) && (val > (smallPrimeTable.int32At(idx)))) {
			idx = idx + 1;
		}
		if (idx >= smallPrimeTable.count()) {
			return (attempt.times(IntegerValue.make(2))).plus(IntegerValue.one());
		} else {
			return IntegerValue.make((smallPrimeTable.int32At(idx)));
		}
		/*
		udanax-top.st:33101:PrimeSizeProvider methodsFor: 'accessing'!
		{IntegerVar} primeAfter: attempt {IntegerVar}
			| val {UInt32} idx {UInt32} lim {UInt32} |
			
			idx _ UInt32Zero.
			val _ attempt DOTasLong.
			lim _ smallPrimeTable count.
			[(idx < lim) and: [val > (smallPrimeTable uIntAt: idx)]] 
				whileTrue: [idx _ idx + 1].
			idx >= smallPrimeTable count
				ifTrue: [^ (attempt * 2) + 1]
				ifFalse: [^ Integer IntegerVar: (smallPrimeTable uIntAt: idx)]!
		*/
	}

	public int uInt32PrimeAfter(int attempt) {
		int idx = 0;
		int val = attempt;
		int lim = smallPrimeTable.count();
		while ((idx < lim) && (val > (smallPrimeTable.int32At(idx)))) {
			idx = idx + 1;
		}
		if (idx >= smallPrimeTable.count()) {
			return (attempt * 2) + 1;
		} else {
			return smallPrimeTable.int32At(idx);
		}
		/*
		udanax-top.st:33114:PrimeSizeProvider methodsFor: 'accessing'!
		{UInt32} uInt32PrimeAfter: attempt {UInt32}
			| val {UInt32} idx {UInt32} lim {UInt32} |
			
			idx _ UInt32Zero.
			val _ attempt.
			lim _ smallPrimeTable count.
			[(idx < lim) and: [val > (smallPrimeTable uIntAt: idx)]] 
				whileTrue: [idx _ idx + 1].
			idx >= smallPrimeTable count
				ifTrue: [^ (attempt * 2) + 1]
				ifFalse: [^ smallPrimeTable uIntAt: idx]!
		*/
	}

	/////////////////////////////////////////////
	// Comparing and Hashing
	
	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		TODOreturn asOop();
		/*
		udanax-top.st:33135:PrimeSizeProvider methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:33137:PrimeSizeProvider methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}

	//	public static void initTimeNonInherited() {
	//		REQUIRES(LPPrimeSizeProvider.getCategory());
	//		/*
	//		udanax-top.st:33153:PrimeSizeProvider class methodsFor: 'smalltalk: initialization'!
	//		initTimeNonInherited
	//			self REQUIRES: LPPrimeSizeProvider.!
	//		*/
	//	}
}
