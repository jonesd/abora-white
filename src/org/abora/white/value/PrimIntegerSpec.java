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
package org.abora.white.value;

import java.math.BigInteger;

import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.IntegerVarArray;
import org.abora.white.collection.arrays.PrimArray;
import org.abora.white.collection.arrays.PrimIntegerArray;
import org.abora.white.collection.arrays.UInt32Array;
import org.abora.white.collection.arrays.UInt8Array;
import org.abora.white.xpp.basic.Heaper;

public class PrimIntegerSpec extends PrimSpec {
	protected int myBitCount;
	protected boolean amSigned;
	protected IntegerValue myMin;
	protected IntegerValue myMax;
	
	public static final int UNLIMITED_BITS = 0; 
	/*
	udanax-top.st:34450:
	PrimSpec subclass: #PrimIntegerSpec
		instanceVariableNames: '
			myBitCount {Int32}
			amSigned {BooleanVar}
			myMin {Int32}
			myMax {Int32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'X++ PrimArrays'!
	*/
	/*
	udanax-top.st:34458:
	(PrimIntegerSpec getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
	*/
	/*
	udanax-top.st:34613:
	PrimIntegerSpec class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:34616:
	(PrimIntegerSpec getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #(COPY xpp ); yourself)!
	*/

	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new specification for integer values. 
	 * 
	 * @param arrayClass the type of <code>PrimArray</code> used to hold values that
	 * 	meet this specification
	 * @param bitCount 
	 * @param isSigned true if values can be negative and positive
	 */
	protected PrimIntegerSpec(Class arrayClass, int bitCount, boolean isSigned) {
		super(arrayClass);
		myBitCount = bitCount;
		amSigned = isSigned;
		if (myBitCount != UNLIMITED_BITS) {
			if (amSigned) {
				BigInteger bigMin = BigInteger.valueOf(2).pow(myBitCount - 1).negate();
				BigInteger bigMax = bigMin.negate().subtract(BigInteger.valueOf(1));
				myMin = IntegerValue.make(bigMin.longValue());
				myMax = IntegerValue.make(bigMax.longValue());
			} else {
				myMin = IntegerValue.zero();
				BigInteger bigMax = BigInteger.valueOf(2).pow(myBitCount).subtract(BigInteger.valueOf(1));
				myMax = IntegerValue.make(bigMax.longValue());
			}
		}
		/*
		udanax-top.st:34488:PrimIntegerSpec methodsFor: 'create'!
		create: primClass {Category} with: bitCount {Int32} with: isSigned {BooleanVar}
			super create: primClass.
			myBitCount := bitCount.
			amSigned := isSigned.
			myBitCount ~~ -1 ifTrue:
				[amSigned ifTrue:
					[[myMin := (2 raisedTo: myBitCount - 1) negated.
					myMax := myMin negated - 1] smalltalkOnly.
					'myMin = 1 << (myBitCount - 1);
					myMax = ~myMin;' translateOnly]
				ifFalse:
					[[myMin := Int32Zero.
					myMax := (2 raisedTo: myBitCount) - 1] smalltalkOnly.
					'myMin = Int32Zero;
					/- the shift is done in two steps to avoid five-bit truncation on SPARCs -/
					myMax = ~(((~Int32Zero) << (myBitCount - 1)) << 1);' translateOnly]]!
		*/
	}


	//////////////////////////////////////////////
	// Accessing

	/**
	 * How many bits, or zero if it is unlimited
	 */
	public int bitCount() {
		return myBitCount;
	}

	/**
	 * Return true if this specifications values may be negative.
	 * 
	 * @return true if this specifications values may be negative.
	 */
	public boolean isSigned() {
		return amSigned;
	}

	/**
	 * Return the maximum value that the specification can hold,
	 * or 0 if unbounded.
	 * 
	 * @return maximum value or 0 if unbounded
	 */
	IntegerValue maximumValue() {
		return myMax;
	}

	/**
	 * Return the minimum value that the specification can hold,
	 * or 0 if unbounded..
	 * 
	 * @return minimum value or 0 if unbounded
	 */
	IntegerValue minimumValue() {
		return myMin;
	}

	/**
	 * Return a specification whose range of values contains the ranges
	 * of both <code>this</code> and <code>other</code>.
	 * 
	 * @param other other range of values to contain
	 * @param combined specification
	 */
	public PrimIntegerSpec combine(PrimIntegerSpec other) {
		if (this == other) {
			return this;
		}
		if (myBitCount == UNLIMITED_BITS) {
			return this;
		}
		if (other.bitCount() == UNLIMITED_BITS) {
			return other;
		}
		if (myBitCount < other.bitCount()) {
			return other;
		}
		if (myBitCount > other.bitCount()) {
			return this;
		}
		if (amSigned == other.isSigned()) {
			return this;
		}
		/* here we get ad hoc since we need to expand to the next larger size */
		if (myBitCount == 8) {
			return PrimSpec.int16();
		}
		if (myBitCount == 16) {
			return PrimSpec.int32();
		}
		if (myBitCount == 32) {
			return PrimSpec.int64();
		}
		return PrimSpec.integerVar();
		/*
		udanax-top.st:34468:PrimIntegerSpec methodsFor: 'accessing'!
		{PrimIntegerSpec} combine: other {PrimIntegerSpec}
			"A spec whose range of values contains both ranges"
			
			self == other ifTrue: [^self].
			myBitCount == Int32Zero ifTrue: [^self].
			other bitCount == Int32Zero ifTrue: [^other].
			myBitCount < other bitCount ifTrue: [^other].
			myBitCount > other bitCount ifTrue: [^self].
			amSigned == other isSigned ifTrue: [^self].
			"here we get ad hoc since we need to expand to the next larger size"
			myBitCount == 8 ifTrue: [^PrimSpec int32].
			^PrimSpec integerVar!
		*/
	}


	//////////////////////////////////////////////
	// Comparing and Hashing

	public int actualHashForEqual() {
		int signPart;
		if (amSigned) {
			signPart = 255;
		} else {
			signPart = 0;
		}
		throw new UnsupportedOperationException();
//TODO		return (getCategory().hashForEqual() ^ (FHash.fastHash(myBitCount))) ^ signPart;
		/*
		udanax-top.st:34508:PrimIntegerSpec methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			| signPart {UInt32} |
			amSigned ifTrue: [signPart _ 255] ifFalse: [signPart _ UInt32Zero].
			^(self getCategory hashForEqual
				bitXor: (FHash fastHash.UInt32: myBitCount))
				bitXor: signPart!
		*/
	}

	/**
	 * Return true if this specification can hold the given <code>value</code>.
	 * 
	 * @param value integer value to test against.
	 * @return true if this specification can hold the given <code>value</code>.
	 */
	public boolean canHold(IntegerValue value) {
		return myBitCount == UNLIMITED_BITS || (value.isGE(myMin) && value.isLE(myMax));
		/*
		udanax-top.st:34515:PrimIntegerSpec methodsFor: 'testing'!
		{BooleanVar} canHold: value {IntegerVar}
			"Whether this spec can hold the given value"
			
			[^myBitCount = -1 or: [value >= myMin and: [value <= myMax]]] smalltalkOnly.
			'if (myBitCount = -1) {
				return TRUE;
			} else if (amSigned) {
				return value >= myMin && value <= myMax;
			} else {
				return (unsigned) value.asLong () >= (unsigned) myMin
					&& (unsigned) value.asLong () <= (unsigned) myMax;
			}' translateOnly!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof PrimIntegerSpec) {
			PrimIntegerSpec spec = (PrimIntegerSpec) other;
			return myBitCount == spec.bitCount() && (amSigned == spec.isSigned());
		} else {
			return false;
		}
		/*
		udanax-top.st:34528:PrimIntegerSpec methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: PrimIntegerSpec into: [ :spec |
				^myBitCount = spec bitCount
					and: [amSigned == spec isSigned]]
			others:
				[^false].
			^ false "compiler fodder"!
		*/
	}


	//////////////////////////////////////////////
	// Array Factory Methods

//	/**
//	 * Make an array initialized to zero values
//	 */
//	public PrimArray array(int count) {
//		if (this == PrimSpec.int8()) {
//			return UInt32Array.make(count);
//		}
//		if (this == PrimSpec.uInt32()) {
//			return UInt32Array.make(count);
//		}
//		if (this == PrimSpec.uInt8()) {
//			return UInt8Array.make(count);
//		}
//		if (this == PrimSpec.int32()) {
//			return Int32Array.make(count);
//		}
//		if (this == PrimSpec.integerVar()) {
//			return IntegerVarArray.zeros(count);
//		}
//		throw new IllegalStateException("BadPrimSpec");
//		/*
//		udanax-top.st:34539:PrimIntegerSpec methodsFor: 'making'!
//		{PrimArray} array: count {Int32 default: Int32Zero}
//			"Make an array initialized to zero values"
//			
//			[self == (PrimSpec uInt32 basicCast: Heaper star) ifTrue: [^UInt32Array make: count].
//			self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array make: count].
//			self == (PrimSpec int32 basicCast: Heaper star) ifTrue: [^Int32Array make: count].
//			self == (PrimSpec integerVar basicCast: Heaper star) ifTrue: [^IntegerVarArray zeros: count].
//			Heaper BLAST: #BadPrimSpec] translateOnly.
//			[^myClass create: count] smalltalkOnly.
//			^ NULL "compiler fodder"!
//		*/
//	}

	/**
	 * Make an array with the values at the given address
	 */
	public PrimArray arrayFromBuffer(Object buffer) {
		if (this == PrimSpec.uInt32()) {
			return UInt32Array.make((int[])buffer);
		}
		if (this == PrimSpec.uInt8()) {
			return UInt8Array.make((short[])buffer);
		}
		if (this == PrimSpec.int32()) {
			return Int32Array.make((int[])buffer);
		}
		if (this == PrimSpec.integerVar()) {
			return IntegerVarArray.make((IntegerValue[])buffer);
		}
		//TODO need rest of PrimSpecs or some reflection/invocation technique
		throw new IllegalStateException("BadPrimSpec");
		/*
		udanax-top.st:34550:PrimIntegerSpec methodsFor: 'making'!
		{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
			"Make an array with the values at the given address"
			
			[self == (PrimSpec uInt32 basicCast: Heaper star) ifTrue: [^UInt32Array make: count with: buffer].
			self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array make: count with: buffer].
			self == (PrimSpec int32 basicCast: Heaper star) ifTrue: [^Int32Array make: count with: buffer].
			self == (PrimSpec integerVar basicCast: Heaper star) ifTrue: [^IntegerVarArray make: count with: buffer].
			Heaper BLAST: #BadPrimSpec] translateOnly.
			[^myClass create: count with: buffer] smalltalkOnly.
			^ NULL "compiler fodder"!
		*/
	}

	/**
	 * Make an array the contents of the string
	 */
	public PrimIntegerArray string(String string) {
		if (this == PrimSpec.uInt8()) {
			return UInt8Array.string(string);
		}
		throw new UnsupportedOperationException();
		/*
		udanax-top.st:34561:PrimIntegerSpec methodsFor: 'making'!
		{PrimIntegerArray} string: string {char star}
			"Make an array the contents of the string"
			
			self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array string: string].
			self unimplemented.
			^NULL "fodder"!
		*/
	}

	/**
	 * A boxed integer value
	 */
	public IntegerValue value(int number) {
		return IntegerValue.make(number);
		/*
		udanax-top.st:34568:PrimIntegerSpec methodsFor: 'making'!
		{PrimIntValue INLINE} value: number {IntegerVar}
			"A boxed integer value"
			
			^PrimIntValue make: number!
		*/
	}

	/**
	 * Make a copy of an array with a different representation size. The arguments are the same
	 * as in PrimArray::copy.
	 */
	public PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset) {
		if (this == PrimSpec.uInt32()) {
			return UInt32Array.make(size, array, start, count, offset);
		}
		if (this == PrimSpec.uInt8()) {
			return UInt8Array.make(size, array, start, count, offset);
		}
		if (this == PrimSpec.int32()) {
			return Int32Array.make(size, array, start, count, offset);
		}
		if (this == PrimSpec.integerVar()) {
			return IntegerVarArray.make(size, array, start, count, offset);
		}
		throw new IllegalStateException("BadPrimSpec");
		/*
		udanax-top.st:34575:PrimIntegerSpec methodsFor: 'private: making'!
		{PrimArray} privateCopy: array {PrimArray}
			with: size {Int32 default: -1}
			with: start {Int32 default: Int32Zero}
			with: count {Int32 default: -1}
			with: offset {Int32 default: Int32Zero}
			"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
			
			[self == (PrimSpec uInt32 basicCast: Heaper star) ifTrue: [^UInt32Array make: size with: array with: start with: count with: offset].
			self == (PrimSpec uInt8 basicCast: Heaper star) ifTrue: [^UInt8Array make: size with: array with: start with: count with: offset].
			self == (PrimSpec int32 basicCast: Heaper star) ifTrue: [^Int32Array make: size with: array with: start with: count with: offset].
			self == (PrimSpec integerVar basicCast: Heaper star) ifTrue: [^IntegerVarArray make: size with: array with: start with: count with: offset].
			Heaper BLAST: #BadPrimSpec] translateOnly.
			[^myClass create: size with: array with: start with: count with: offset] smalltalkOnly.
			^ NULL "compiler fodder"!
		*/
	}

	//	public int precision() {
	//		passe()
	//		/* bitCount */;
	//		/*
	//		udanax-top.st:34592:PrimIntegerSpec methodsFor: 'smalltalk: passe'!
	//		{Int32} precision
	//			self passe "bitCount"!
	//		*/
	//	}

	//	public PrimIntegerSpec(Rcvr receiver) {
	//		super(receiver);
	//		myBitCount = receiver.receiveInt32();
	//		amSigned = receiver.receiveBooleanVar();
	//		myMin = receiver.receiveInt32();
	//		myMax = receiver.receiveInt32();
	//		/*
	//		udanax-top.st:34598:PrimIntegerSpec methodsFor: 'generated:'!
	//		create.Rcvr: receiver {Rcvr}
	//			super create.Rcvr: receiver.
	//			myBitCount _ receiver receiveInt32.
	//			amSigned _ receiver receiveBooleanVar.
	//			myMin _ receiver receiveInt32.
	//			myMax _ receiver receiveInt32.!
	//		*/
	//	}

	//	public void sendSelfTo(Xmtr xmtr) {
	//		super.sendSelfTo(xmtr);
	//		xmtr.sendInt32(myBitCount);
	//		xmtr.sendBooleanVar(amSigned);
	//		xmtr.sendInt32(myMin);
	//		xmtr.sendInt32(myMax);
	//		/*
	//		udanax-top.st:34605:PrimIntegerSpec methodsFor: 'generated:'!
	//		{void} sendSelfTo: xmtr {Xmtr}
	//			super sendSelfTo: xmtr.
	//			xmtr sendInt32: myBitCount.
	//			xmtr sendBooleanVar: amSigned.
	//			xmtr sendInt32: myMin.
	//			xmtr sendInt32: myMax.!
	//		*/
	//	}

	//	/**
	//	 * {BooleanVar CLIENT} isSigned
	//	 * {Int32 CLIENT} precision
	//	 * {PrimIntegerArray CLIENT} string: string {char star}
	//	 * {PrimInteger CLIENT} value: number {IntegerVar}
	//	 */
	//	public static void info() {
	//		/*
	//		udanax-top.st:34621:PrimIntegerSpec class methodsFor: 'smalltalk: system'!
	//		info.stProtocol
	//		"{BooleanVar CLIENT} isSigned
	//		{Int32 CLIENT} precision
	//		{PrimIntegerArray CLIENT} string: string {char star}
	//		{PrimInteger CLIENT} value: number {IntegerVar}
	//		"!
	//		*/
	//	}
}
