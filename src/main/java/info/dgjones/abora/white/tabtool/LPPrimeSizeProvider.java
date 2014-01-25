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
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * This is a non-stepper stepper that returns a stream of prime numbers.
 * SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of
 * increased table size, LPPrimeSizeProvider does not claim to do this.
 * - michael 31 July 1991
 */
public class LPPrimeSizeProvider extends PrimeSizeProvider {
	protected static LPPrimeSizeProvider MySoleProvider = new LPPrimeSizeProvider(primeTable());
	;
	/*
	udanax-top.st:33157:
	PrimeSizeProvider subclass: #LPPrimeSizeProvider
		instanceVariableNames: ''
		classVariableNames: 'MySoleProvider {LPPrimeSizeProvider} '
		poolDictionaries: ''
		category: 'Xanadu-tabtool'!
	*/
	/*
	udanax-top.st:33161:
	LPPrimeSizeProvider comment:
	'This is a non-stepper stepper that returns a stream of prime numbers.
	SCPrimeSizeProvider rejects many primes to be nice for secondary clustering at the cost of increased table size, LPPrimeSizeProvider does not claim to do this.
	 - michael 31 July 1991'!
	*/
	/*
	udanax-top.st:33167:
	(LPPrimeSizeProvider getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/
	/*
	udanax-top.st:33182:
	LPPrimeSizeProvider class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:33185:
	(LPPrimeSizeProvider getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected LPPrimeSizeProvider(Int32Array aSmallPrimeTable) {
		super(aSmallPrimeTable);
		/*
		udanax-top.st:33172:LPPrimeSizeProvider methodsFor: 'creation'!
		create: aSmallPrimeTable {UInt32Array}
			super create: aSmallPrimeTable!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	public static PrimeSizeProvider make() {
		return MySoleProvider;
		/*
		udanax-top.st:33190:LPPrimeSizeProvider class methodsFor: 'make'!
		{LPPrimeSizeProvider INLINE} make
			^ MySoleProvider!
		*/
	}

	/////////////////////////////////////////////
	// Comparing and Hashing

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		return asOop();
		/*
		udanax-top.st:33177:LPPrimeSizeProvider methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:33179:LPPrimeSizeProvider methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}

	/////////////////////////////////////////////
	// Initialization

	public static Int32Array primeTable() {
		Int32Array smallPrimeTable;
		smallPrimeTable = Int32Array.make(71);
		smallPrimeTable.storeInt32(0, 7);
		smallPrimeTable.storeInt32(1, 19);
		smallPrimeTable.storeInt32(2, 41);
		smallPrimeTable.storeInt32(3, 67);
		smallPrimeTable.storeInt32(4, 101);
		smallPrimeTable.storeInt32(5, 139);
		smallPrimeTable.storeInt32(6, 191);
		smallPrimeTable.storeInt32(7, 241);
		smallPrimeTable.storeInt32(8, 313);
		smallPrimeTable.storeInt32(9, 401);
		smallPrimeTable.storeInt32(10, 499);
		smallPrimeTable.storeInt32(11, 617);
		smallPrimeTable.storeInt32(12, 751);
		smallPrimeTable.storeInt32(13, 911);
		smallPrimeTable.storeInt32(14, 1091);
		smallPrimeTable.storeInt32(15, 1297);
		smallPrimeTable.storeInt32(16, 1543);
		smallPrimeTable.storeInt32(17, 1801);
		smallPrimeTable.storeInt32(18, 2113);
		smallPrimeTable.storeInt32(19, 2459);
		smallPrimeTable.storeInt32(20, 2851);
		smallPrimeTable.storeInt32(21, 3331);
		smallPrimeTable.storeInt32(22, 3833);
		smallPrimeTable.storeInt32(23, 4421);
		smallPrimeTable.storeInt32(24, 5059);
		smallPrimeTable.storeInt32(25, 5801);
		smallPrimeTable.storeInt32(26, 6607);
		smallPrimeTable.storeInt32(27, 7547);
		smallPrimeTable.storeInt32(28, 8599);
		smallPrimeTable.storeInt32(29, 9697);
		smallPrimeTable.storeInt32(30, 11004);
		smallPrimeTable.storeInt32(31, 12479);
		smallPrimeTable.storeInt32(32, 14057);
		smallPrimeTable.storeInt32(33, 15803);
		smallPrimeTable.storeInt32(34, 17881);
		smallPrimeTable.storeInt32(35, 20117);
		smallPrimeTable.storeInt32(36, 22573);
		smallPrimeTable.storeInt32(37, 28499);
		smallPrimeTable.storeInt32(38, 32003);
		smallPrimeTable.storeInt32(39, 35759);
		smallPrimeTable.storeInt32(40, 40009);
		smallPrimeTable.storeInt32(41, 44729);
		smallPrimeTable.storeInt32(42, 50053);
		smallPrimeTable.storeInt32(43, 55933);
		smallPrimeTable.storeInt32(44, 62483);
		smallPrimeTable.storeInt32(45, 69911);
		smallPrimeTable.storeInt32(46, 77839);
		smallPrimeTable.storeInt32(47, 86929);
		smallPrimeTable.storeInt32(48, 96787);
		smallPrimeTable.storeInt32(49, 108041);
		smallPrimeTable.storeInt32(50, 120473);
		smallPrimeTable.storeInt32(51, 134087);
		smallPrimeTable.storeInt32(52, 149287);
		smallPrimeTable.storeInt32(53, 166303);
		smallPrimeTable.storeInt32(54, 185063);
		smallPrimeTable.storeInt32(55, 205957);
		smallPrimeTable.storeInt32(56, 228887);
		smallPrimeTable.storeInt32(57, 254663);
		smallPrimeTable.storeInt32(58, 282833);
		smallPrimeTable.storeInt32(59, 313979);
		smallPrimeTable.storeInt32(60, 347287);
		smallPrimeTable.storeInt32(61, 384317);
		smallPrimeTable.storeInt32(62, 424667);
		smallPrimeTable.storeInt32(63, 468841);
		smallPrimeTable.storeInt32(64, 517073);
		smallPrimeTable.storeInt32(65, 569927);
		smallPrimeTable.storeInt32(66, 627553);
		smallPrimeTable.storeInt32(67, 691183);
		smallPrimeTable.storeInt32(68, 760657);
		smallPrimeTable.storeInt32(69, 836483);
		smallPrimeTable.storeInt32(70, 919757);
		return smallPrimeTable;
		/*
		udanax-top.st:33195:LPPrimeSizeProvider class methodsFor: 'initialization'!
		{UInt32Array} primeTable
			| smallPrimeTable {UInt32Array} |
			smallPrimeTable _ UInt32Array make: 71.
			smallPrimeTable at: UInt32Zero storeUInt: 7.
			smallPrimeTable at: 1 storeUInt: 19.
			smallPrimeTable at: 2 storeUInt: 41.
			smallPrimeTable at: 3 storeUInt: 67.
			smallPrimeTable at: 4 storeUInt: 101.
			smallPrimeTable at: 5 storeUInt: 139.
			smallPrimeTable at: 6 storeUInt: 191.
			smallPrimeTable at: 7 storeUInt: 241.
			smallPrimeTable at: 8 storeUInt: 313.
			smallPrimeTable at: 9 storeUInt: 401.
			smallPrimeTable at: 10 storeUInt: 499.
			smallPrimeTable at: 11 storeUInt: 617.
			smallPrimeTable at: 12 storeUInt: 751.
			smallPrimeTable at: 13 storeUInt: 911.
			smallPrimeTable at: 14 storeUInt: 1091.
			smallPrimeTable at: 15 storeUInt: 1297.
			smallPrimeTable at: 16 storeUInt: 1543.
			smallPrimeTable at: 17 storeUInt: 1801.
			smallPrimeTable at: 18 storeUInt: 2113.
			smallPrimeTable at: 19 storeUInt: 2459.
			smallPrimeTable at: 20 storeUInt: 2851.
			smallPrimeTable at: 21 storeUInt: 3331.
			smallPrimeTable at: 22 storeUInt: 3833.
			smallPrimeTable at: 23 storeUInt: 4421.
			smallPrimeTable at: 24 storeUInt: 5059.
			smallPrimeTable at: 25 storeUInt: 5801.
			smallPrimeTable at: 26 storeUInt: 6607.
			smallPrimeTable at: 27 storeUInt: 7547.
			smallPrimeTable at: 28 storeUInt: 8599.
			smallPrimeTable at: 29 storeUInt: 9697.
			smallPrimeTable at: 30 storeUInt: 11004.
			smallPrimeTable at: 31 storeUInt: 12479.
			smallPrimeTable at: 32 storeUInt: 14057.
			smallPrimeTable at: 33 storeUInt: 15803.
			smallPrimeTable at: 34 storeUInt: 17881.
			smallPrimeTable at: 35 storeUInt: 20117.
			smallPrimeTable at: 36 storeUInt: 22573.
			smallPrimeTable at: 37 storeUInt: 28499.
			smallPrimeTable at: 38 storeUInt: 32003.
			smallPrimeTable at: 39 storeUInt: 35759.
			smallPrimeTable at: 40 storeUInt: 40009.
			smallPrimeTable at: 41 storeUInt: 44729.
			smallPrimeTable at: 42 storeUInt: 50053.
			smallPrimeTable at: 43 storeUInt: 55933.
			smallPrimeTable at: 44 storeUInt: 62483.
			smallPrimeTable at: 45 storeUInt: 69911.
			smallPrimeTable at: 46 storeUInt: 77839.
			smallPrimeTable at: 47 storeUInt: 86929.
			smallPrimeTable at: 48 storeUInt: 96787.
			smallPrimeTable at: 49 storeUInt: 108041.
			smallPrimeTable at: 50 storeUInt: 120473.
			smallPrimeTable at: 51 storeUInt: 134087.
			smallPrimeTable at: 52 storeUInt: 149287.
			smallPrimeTable at: 53 storeUInt: 166303.
			smallPrimeTable at: 54 storeUInt: 185063.
			smallPrimeTable at: 55 storeUInt: 205957.
			smallPrimeTable at: 56 storeUInt: 228887.
			smallPrimeTable at: 57 storeUInt: 254663.
			smallPrimeTable at: 58 storeUInt: 282833.
			smallPrimeTable at: 59 storeUInt: 313979.
			smallPrimeTable at: 60 storeUInt: 347287.
			smallPrimeTable at: 61 storeUInt: 384317.
			smallPrimeTable at: 62 storeUInt: 424667.
			smallPrimeTable at: 63 storeUInt: 468841.
			smallPrimeTable at: 64 storeUInt: 517073.
			smallPrimeTable at: 65 storeUInt: 569927.
			smallPrimeTable at: 66 storeUInt: 627553.
			smallPrimeTable at: 67 storeUInt: 691183.
			smallPrimeTable at: 68 storeUInt: 760657.
			smallPrimeTable at: 69 storeUInt: 836483.
			smallPrimeTable at: 70 storeUInt: 919757.
			^ smallPrimeTable!
		*/
	}

	//	public static void linkTimeNonInherited() {
	//		MySoleProvider = null;
	//		/*
	//		udanax-top.st:33279:LPPrimeSizeProvider class methodsFor: 'smalltalk: initialization'!
	//		linkTimeNonInherited
	//			MySoleProvider _ NULL.!
	//		*/
	//	}
}
