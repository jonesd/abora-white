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

import java.io.PrintWriter;
import java.math.BigInteger;

import org.abora.white.hash.FHash;
import org.abora.white.xpp.basic.Heaper;

public class IntegerValue extends PrimIntValue implements Comparable {
	//TODO This is surely a performance disaster. Couldnt we just use int/long?
	private final BigInteger value;

	private static final IntegerValue ZERO = new IntegerValue(0);
	private static final IntegerValue ONE = new IntegerValue(1);

	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new instance with value.
	 * 
	 * @param value 
	 */
	protected IntegerValue(long value) {
		super();
		this.value = BigInteger.valueOf(value);
	}

	/**
	 * Construct a new instance with value.
	 * 
	 * @param value 
	 */
	protected IntegerValue(BigInteger value) {
		super();
		this.value = value;
	}

	//////////////////////////////////////////////
	// Static Method Factories
	
	public static IntegerValue zero() {
		return ZERO;
	}

	public static IntegerValue one() {
		return ONE;
	}

	public static IntegerValue make(long value) {
		return new IntegerValue(value);
	}

	public static IntegerValue make(BigInteger value) {
		return new IntegerValue(value);
	}


	//////////////////////////////////////////////
	// Comparing and Hashing

	public boolean isEqual(Heaper other) {
		if (other instanceof IntegerValue) {
			IntegerValue o = (IntegerValue) other;
			return value.equals(o.value);
		} else {
			//TODO what about floats/etc?
			return false;
		}
	}

	public int compareTo(Object other) {
		IntegerValue o = (IntegerValue) other;
		return value.compareTo(o.value);
	}


	/**
	 * Return true if the first number is greater than or equal to the second number.
	 */
	public boolean isGE(IntegerValue another) {
		return compareTo(another) >= 0;
	}

	/**
	 * Return true if the first number is less than the second number.
	 */
	public boolean isLT(IntegerValue another) {
		return compareTo(another) < 0;
	}

	/**
	 * Return true if the first number is less than or equal to the second number.
	 */
	public boolean isLE(IntegerValue another) {
		return compareTo(another) <= 0;
	}


	//////////////////////////////////////////////
	// Conversions

	public int intValue() {
		//TODO only here to fit in with BigInteger. Do we need it?
		return asInt32();
	}

	public long longValue() {
		//TODO only here to fit in with BigInteger. Do we need it?
		return asInt64();
	}

	/**
	 * The value as a 8 bit signed integer
	 */
	public byte asInt8() {
		return value.byteValue();
	}

	/**
	 * The value as a 16 bit signed integer
	 */
	public short asInt16() {
		return value.shortValue();
	}

	/**
	 * The value as a 32 bit signed integer
	 */
	public int asInt32() {
		return value.intValue();
	}

	/**
	 * The value as a 64 bit signed integer
	 */
	public long asInt64() {
		return value.longValue();
	}
	
	/**
	 * The value as a BooleanVar.
	 */
	public boolean asBooleanVar() {
		return value.compareTo(BigInteger.ZERO) != 0;
	}

	/**
	 * Return the value as a 32 bit unsigned integer.
	 * 
	 * @return value as a 32 bit unsigned integer.
	 */
	public long asUInt32() {
		return value.longValue() & 0xffffffffL;
	}

	/**
	 * Return the value as a 16 bit unsigned integer.
	 * 
	 * @return value as a 16 bit unsigned integer.
	 */
	public char asUInt16() {
		return (char)(value.intValue() & 0xffff);
	}

	/**
	 * Return the value as an 8 bit unsigned integer.
	 * 
	 * @return value as an 8 bit unsigned integer.
	 */
	public short asUInt8() {
		return (short)(value.intValue() & 0xff);
	}


	//////////////////////////////////////////////
	// Arithmetic Operations
	
	/**
	 * Return the the first number bitwise and'd with the second.
	 */
	public IntegerValue bitwiseAnd(IntegerValue another) {
		return new IntegerValue(value.and(another.value));
		/*
		udanax-top.st:35022:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} bitwiseAnd: another {PrimIntValue}
			"Return the the first number bitwise and'd with the second."
		
			^myValue bitAnd: another asIntegerVar!
		*/
	}

	/**
	 * Return the the first number bitwise or'd with the second.
	 */
	public IntegerValue bitwiseOr(IntegerValue another) {
		return new IntegerValue(value.or(another.value));
		/*
		udanax-top.st:35027:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} bitwiseOr: another {PrimIntValue}
			"Return the the first number bitwise or'd with the second."
		
			^myValue bitOr: another asIntegerVar!
		*/
	}

	/**
	 * Return the the first number bitwise xor'd with the second.
	 */
	public IntegerValue bitwiseXor(IntegerValue another) {
		return new IntegerValue(value.xor(another.value));
		/*
		udanax-top.st:35032:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} bitwiseXor: another {PrimIntValue}
			"Return the the first number bitwise xor'd with the second."
		
			^myValue bitXor: another asIntegerVar!
		*/
	}

	/**
	 * Integer divide the two numbers and return the result.  This truncates.
	 */
	public IntegerValue dividedBy(IntegerValue another) {
		//TODO does this actually match the advertised and Smalltalk behaviour?
		return new IntegerValue(value.divide(another.value));
		/*
		udanax-top.st:35037:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} dividedBy: another {PrimIntValue}
			"Integer divide the two numbers and return the result.  This truncates."
		
			^myValue // another asIntegerVar!
		*/
	}
	/**
	 * Return the the first number shifted to the left by the second amount.
	 */
	public IntegerValue leftShift(IntegerValue another) {
		//TODO should this just have an int parameter?
		return new IntegerValue(value.shiftLeft(another.asInt32()));
		/*
		udanax-top.st:35047:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} leftShift: another {PrimIntValue}
			"Return the the first number shifted to the left by the second amount."
		
			^myValue bitShift: another asIntegerVar!
		*/
	}

	/**
	 * Return the largest of the two numbers.
	 */
	public IntegerValue maximum(IntegerValue another) {
		//TODO rename to max for Java compatability?
		//TODO efficiency improvement by returning maximum rather than re-creating
		return new IntegerValue(value.max(another.value));
		/*
		udanax-top.st:35052:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} maximum: another {PrimIntValue}
			"Return the largest of the two numbers."
		
			^myValue max: another asIntegerVar!
		*/
	}

	/**
	 * Return the smallest of the two numbers.
	 */
	public IntegerValue minimum(IntegerValue another) {
		//TODO rename to min for Java compatability?
		//TODO efficiency improvement by returning minimum rather than re-creating
		return new IntegerValue(value.min(another.value));
		/*
		udanax-top.st:35057:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} minimum: another {PrimIntValue}
			"Return the smallest of the two numbers."
		
			^myValue min: another asIntegerVar!
		*/
	}

	/**
	 * Return the difference two numbers.
	 */
	public IntegerValue minus(IntegerValue another) {
		return new IntegerValue(value.subtract(another.value));
		/*
		udanax-top.st:35062:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} minus: another {PrimIntValue}
			"Return the difference two numbers."
		
			^myValue - another asIntegerVar!
		*/
	}

	/**
	 * Return the the first number modulo the second.
	 */
	public IntegerValue mod(IntegerValue another) {
		return new IntegerValue(value.mod(another.value));
		/*
		udanax-top.st:35067:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} mod: another {PrimIntValue}
			"Return the the first number modulo the second."
		
			^myValue \\ another asIntegerVar!
		*/
	}

	/**
	 * Return the sum of two numbers.
	 */
	public IntegerValue plus(IntegerValue another) {
		// TODO should we rename to add to match java?
		return new IntegerValue(value.add(another.value));
		/*
		udanax-top.st:35072:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} plus: another {PrimIntValue}
			"Return the sum of two numbers."
		
			^myValue + another asIntegerVar!
		*/
	}

	/**
	 * Multiply the two numbers and return the result.
	 */
	public IntegerValue times(IntegerValue another) {
		//TODO should we rename to multiply to match java?
		return new IntegerValue(value.multiply(another.value));
		/*
		udanax-top.st:35077:PrimIntValue methodsFor: 'operations'!
		{IntegerVar CLIENT login} times: another {PrimIntValue}
			"Multiply the two numbers and return the result."
		
			^myValue * another asIntegerVar!
		*/
	}

//	/**
//	 * The value as an indefinite precision integer
//	 */
//	public int asIntegerVar() {
//		return myValue;
//		/*
//		udanax-top.st:35094:PrimIntValue methodsFor: 'accessing'!
//		{IntegerVar INLINE} asIntegerVar
//			"The value as an indefinite precision integer"
//		
//			^myValue!
//		*/
//	}

	/**
	 * What precision is it, in terms of the number of bits used to represent it.  In the
	 * interests of efficiency, this may return a number larger than that *needed* to represent
	 * it.  However, the precision reported must be at least that needed to represent this
	 * number.
	 * The fact that this message is allowed to overestimate precision doesn't interfere with
	 * equality: a->isEqual(b) exactly when they represent that same real number, even if one of
	 * them happens to overestimate precision more that the other.
	 */
	public int bitCount() {
		int precision = (PrimSpec.toHold(this)).bitCount();
		if (precision == 0) {
			throw new IllegalStateException("NoBitCountLimit");
		}
		return precision;
		/*
		udanax-top.st:35109:PrimIntValue methodsFor: 'accessing'!
		{Int32 CLIENT} bitCount
			"What precision is it, in terms of the number of bits used to represent it.  In the interests of efficiency, this may return a number larger than that *needed* to represent it.  However, the precision reported must be at least that needed to represent this number.
			The fact that this message is allowed to overestimate precision doesn't interfere with equality: a->isEqual(b) exactly when they represent that same real number, even if one of them happens to overestimate precision more that the other."
		
			| precision {Int32} |
			precision _ (PrimSpec toHold: myValue) bitCount.
			precision == Int32Zero ifTrue: [Heaper BLAST: #NoBitCountLimit].
			^precision!
		*/
	}

	public int actualHashForEqual() {
		return FHash.hashInt(value.hashCode());
	}

	public void printContentsOn(PrintWriter oo) {
		oo.print(value);
		/*
		udanax-top.st:35137:PrimIntValue methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << myValue << ')'!
		*/
	}
	
	public String toString() {
		return value.toString();
	}

	public BigInteger toBigInteger() {
		return value;
	}

//	public int precision() {
//		passe()
//		/* bitCount */;
//		/*
//		udanax-top.st:35143:PrimIntValue methodsFor: 'smalltalk: passe'!
//		{Int32} precision
//			self passe "bitCount"!
//		*/
//	}

//	public PrimIntValue(Rcvr receiver) {
//		super(receiver);
//		myValue = receiver.receiveIntegerVar();
//		/*
//		udanax-top.st:35149:PrimIntValue methodsFor: 'generated:'!
//		create.Rcvr: receiver {Rcvr}
//			super create.Rcvr: receiver.
//			myValue _ receiver receiveIntegerVar.!
//		*/
//	}

//	public void sendSelfTo(Xmtr xmtr) {
//		super.sendSelfTo(xmtr);
//		xmtr.sendIntegerVar(myValue);
//		/*
//		udanax-top.st:35153:PrimIntValue methodsFor: 'generated:'!
//		{void} sendSelfTo: xmtr {Xmtr}
//			super sendSelfTo: xmtr.
//			xmtr sendIntegerVar: myValue.!
//		*/
//	}

//	public static Heaper make(int value) {
//		return new PrimIntValue(value);
//		/*
//		udanax-top.st:35166:PrimIntValue class methodsFor: 'create'!
//		make: value {IntegerVar}
//			^ self create: value!
//		*/
//	}

//	/**
//	 * {IntegerVar CLIENT} asIntegerVar
//	 */
//	public static void info() {
//		/*
//		udanax-top.st:35171:PrimIntValue class methodsFor: 'smalltalk: system'!
//		info.stProtocol
//		"{IntegerVar CLIENT} asIntegerVar
//		"!
//		*/
//	}
}
