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

import org.abora.white.collection.arrays.IEEE32Array;
import org.abora.white.collection.arrays.IEEE64Array;
import org.abora.white.collection.arrays.Int16Array;
import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.Int64Array;
import org.abora.white.collection.arrays.Int8Array;
import org.abora.white.collection.arrays.IntegerVarArray;
import org.abora.white.collection.arrays.PrimArray;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.arrays.SharedPtrArray;
import org.abora.white.collection.arrays.UInt16Array;
import org.abora.white.collection.arrays.UInt32Array;
import org.abora.white.collection.arrays.UInt8Array;
import org.abora.white.xpp.basic.Heaper;

/**
 * A specification of a kind of primitive data type which can be stored in PrimArrays. It
 * gives you protocol for creating and copying PrimArrays. The class and characteristics of
 * this object determine what kind of things are stored there, and how much precision they
 * have.
 */
public abstract class PrimSpec extends Heaper {
	protected Class arrayClass;

	private final static PrimFloatSpec IEEE32_SPEC = new PrimFloatSpec(IEEE32Array.class, 32);
	private final static PrimFloatSpec IEEE64_SPEC = new PrimFloatSpec(IEEE64Array.class, 64);

	private final static PrimIntegerSpec INT8_SPEC = new PrimIntegerSpec(Int8Array.class, 8, true);
	private final static PrimIntegerSpec INT16_SPEC = new PrimIntegerSpec(Int16Array.class, 16, true);
	private final static PrimIntegerSpec INT32_SPEC = new PrimIntegerSpec(Int32Array.class, 32, true);
	private final static PrimIntegerSpec INT64_SPEC = new PrimIntegerSpec(Int64Array.class, 64, true);
	private final static PrimIntegerSpec INTEGER_VAR_SPEC = new PrimIntegerSpec(IntegerVarArray.class, PrimIntegerSpec.UNLIMITED_BITS, true);
	private final static PrimIntegerSpec UINT8_SPEC = new PrimIntegerSpec(UInt8Array.class, 8, false);
	private final static PrimIntegerSpec UINT16_SPEC = new PrimIntegerSpec(UInt16Array.class, 16, false);
	private final static PrimIntegerSpec UINT32_SPEC = new PrimIntegerSpec(UInt32Array.class, 32, false);

	private final static PrimPointerSpec PTR_SPEC = new PrimPointerSpec(PtrArray.class);
	private final static PrimPointerSpec SHARED_PTR_SPEC = new PrimPointerSpec(SharedPtrArray.class);

	//////////////////////////////////////////////
	// Constructors

	/**
	 * Construct a new specification.
	 * 
	 * @param arrayClass the type of <code>PrimArray</code> used to hold values that
	 * 	meet this specification
	 */
	protected PrimSpec(Class arrayClass) {
		super();
		this.arrayClass = arrayClass;
	}

	//////////////////////////////////////////////
	// Static Factory Methods

	/**
	 * Return the specification defining IEEE32 floating-point numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimFloatSpec iEEE32() {
		return IEEE32_SPEC;
	}

	/**
	 * Return the specification defining IEEE64 floating-point numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimFloatSpec iEEE64() {
		return IEEE64_SPEC;
	}

	/**
	 * Return the specification suitable for floating-point numbers
	 * of the specified <code>bitCount</code> in size.
	 * An exact size match is required.
	 * 
	 * @param bitCount number of bits the specifications values have..
	 * @return the matching specification.
	 * @throws  UnsupportedOperationException if the specified <code>bitCount</code> can not be precisely matched.
	 */
	public static PrimFloatSpec iEEE(int bitCount) {
		switch (bitCount) {
			case 32 :
				return iEEE32();
			case 64 :
				return iEEE64();
			default :
				throw new UnsupportedOperationException();
		}
	}

	/**
	 * Return the specification defining signed unlimited size integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec integerVar() {
		return INTEGER_VAR_SPEC;
	}

	/**
	 * Return the specification defining signed 8-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec int8() {
		return INT8_SPEC;
	}

	/**
	 * Return the specification defining signed 16-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec int16() {
		return INT16_SPEC;
	}

	/**
	 * Return the specification defining signed 32-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec int32() {
		return INT32_SPEC;
	}

	/**
	 * Return the specification defining signed 64-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec int64() {
		return INT64_SPEC;
	}

	/**
	 * Return the specification suitable for signed integer numbers
	 * of the specified <code>bitCount</code> in size.
	 * An exact size match is required.
	 * 
	 * @param bitCount number of bits the specifications values have..
	 * @return the matching specification.
	 * @throws  UnsupportedOperationException if the specified <code>bitCount</code> is not available.
	 */
	public static PrimIntegerSpec signedInteger(int bitCount) {
		switch (bitCount) {
			case 8 :
				return int8();
			case 16 :
				return int16();
			case 32 :
				return int32();
			case 64 :
				return int64();
			default :
				throw new UnsupportedOperationException();
		}
	}

	/**
	 * Return the specification defining un-signed 8-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec uInt8() {
		return UINT8_SPEC;
	}

	/**
	 * Return the specification defining un-signed 16-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec uInt16() {
		return UINT16_SPEC;
	}

	/**
	 * Return the specification defining un-signed 32-bit integer numbers.
	 * 
	 * @return the specification.
	 */
	public static PrimIntegerSpec uInt32() {
		return UINT32_SPEC;
	}

	/**
	 * Return the specification suitable for un-signed integer numbers
	 * of the specified <code>bitCount</code> in size.
	 * An exact size match is required.
	 * 
	 * @param bitCount number of bits the specifications values have..
	 * @return the matching specification.
	 * @throws  UnsupportedOperationException if the specified <code>bitCount</code> is not available.
	 */
	public static PrimIntegerSpec unsignedInteger(int bitCount) {
		switch (bitCount) {
			case 8 :
				return uInt8();
			case 16 :
				return uInt16();
			case 32 :
				return uInt32();
			default :
				throw new UnsupportedOperationException();
		}
	}

	/**
	 * Return the least demanding integer spec that will hold <code>value</code>.
	 * 
	 * @param value integer value to test against.
	 * @return the matching specification.
	 */
	public static PrimIntegerSpec toHold(IntegerValue value) {
		if (value.isLT(IntegerValue.zero())) {
			if (value.isGE(int8().minimumValue())) {
				return int8();
			} else if (value.isGE(int16().minimumValue())) {
				return int16();
			} else if (value.isGE(int32().minimumValue())) {
				return int32();
			} else if (value.isGE(int64().minimumValue())) {
				return int64();
			} else {
				return integerVar();
			}
		} else {
			if (value.isLE(int8().maximumValue())) {
				return int8();
			} else if (value.isLE(uInt8().maximumValue())) {
				return uInt8();
			} else if (value.isLE(int16().maximumValue())) {
				return int16();
			} else if (value.isLE(uInt16().maximumValue())) {
				return uInt16();
			} else if (value.isLE(int32().maximumValue())) {
				return int32();
			} else if (value.isLE(uInt32().maximumValue())) {
				return uInt32();
			} else if (value.isLE(int64().maximumValue())) {
				return int64();
			} else {
				return integerVar();
			}
		}
		/*
			udanax-top.st:34288:PrimSpec class methodsFor: 'pseudo constructors'!
			{PrimIntegerSpec} toHold: value {IntegerVar}
				"The least demanding spec that will hold the given value"
				
				value < IntegerVar0
					ifTrue: [value < Int32Min
						ifTrue: [^self integerVar]
						ifFalse: [^self int32]]
					ifFalse: [value <= Int32Max
						ifTrue: [value <= UInt8Max
							ifTrue: [^self uInt8]
							ifFalse: [^self int32]]
						ifFalse: [value <= UInt32Max
							ifTrue: [^self uInt32]
							ifFalse: [^self integerVar]]]!
			*/
	}

	/**
	 * Return the specification defining pointers to Heaper obects.
	 * 
	 * @return the specification.
	 */
	public static PrimPointerSpec pointer() {
		return PTR_SPEC;
	}
	
	/**
	 * Return the specification defining pointers to Heaper obects where
	 * a count of the number of shared users are kept.
	 * 
	 * @return the specification.
	 */
	public static PrimPointerSpec sharedPointer() {
		return SHARED_PTR_SPEC;
	}
	
	
	//////////////////////////////////////////////
	// Accessing
	
	protected Class arrayClass() {
		return arrayClass;
		/*
			udanax-top.st:34118:PrimSpec methodsFor: 'protected:'!
			{Category INLINE} arrayClass
				^myClass!
			*/
	}
	
	//////////////////////////////////////////////
	// Array Factory Methods
	
	/**
	 * Make an array initialized to some reasonable zero value
	 */
	public abstract PrimArray array(int count);
	/*
	udanax-top.st:34129:PrimSpec methodsFor: 'making'!
	{PrimArray} array: count {Int32 default: Int32Zero}
		"Make an array initialized to some reasonable zero value"
		
		self subclassResponsibility!
	*/

	public PrimArray array() {
		return array(0);
	}
	
	/**
	 * Make an array with the values at the given address
	 */
	public abstract PrimArray arrayFromBuffer(Object buffer);
	/*
	udanax-top.st:34134:PrimSpec methodsFor: 'making'!
	{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
		"Make an array with the values at the given address"
		self subclassResponsibility!
	*/
	
	/**
	 * Make a single element array containing the given value
	 */
	public PrimArray arrayWith(Heaper value) {
		PrimArray result = array(1);
		result.storeValue(0, value);
		return result;
		/*
		udanax-top.st:34139:PrimSpec methodsFor: 'making'!
		{PrimArray} arrayWith: value {Heaper}
			"Make a single element array containing the given value"
			
			| result {PrimArray} |
			result _ self array: 1.
			result at: Int32Zero storeValue: value.
			^result!
		*/
	}
	
	/**
	 * Make a two element array containing the given values
	 */
	public PrimArray arrayWithThree(Heaper value, Heaper other, Heaper another) {
		PrimArray result = array(3);
		result.storeValue(0, value);
		result.storeValue(1, other);
		result.storeValue(2, another);
		return result;
		/*
		udanax-top.st:34147:PrimSpec methodsFor: 'making'!
		{PrimArray} arrayWithThree: value {Heaper} with: other {Heaper} with: another {Heaper}
			"Make a two element array containing the given values"
			
			| result {PrimArray} |
			result _ self array: 3.
			result at: Int32Zero storeValue: value.
			result at: 1 storeValue: other.
			result at: 2 storeValue: another.
			^ result!
		*/
	}
	
	/**
	 * Make a two element array containing the given values
	 */
	public PrimArray arrayWithTwo(Heaper value, Heaper other) {
		PrimArray result = array(2);
		result.storeValue(0, value);
		result.storeValue(1, other);
		return result;
		/*
		udanax-top.st:34157:PrimSpec methodsFor: 'making'!
		{PrimArray} arrayWithTwo: value {Heaper} with: other {Heaper}
			"Make a two element array containing the given values"
			
			| result {PrimArray} |
			result _ self array: 2.
			result at: Int32Zero storeValue: value.
			result at: 1 storeValue: other.
			^ result.!
		*/
	}
	
	//////////////////////////////////////////////
	// Copying
	
	public PrimArray copy(PrimArray array) {
		return copy(array, -1, 0, 0, 0);
	}

	public PrimArray copy(PrimArray array, int count) {
		return copy(array, count, 0, 0, 0);
	}

	public PrimArray copy(PrimArray array, int count, int start) {
		return copy(array, count, start, 0, 0);
	}

	public PrimArray copy(PrimArray array, int count, int start, int before) {
		return copy(array, count, start, before, 0);
	}
	
	/**
	 * Make a copy of an array with a different representation size. The arguments are the same
	 * as in PrimArray::copy.
	 */
	public PrimArray copy(PrimArray array, int count, int start, int before, int after) {
		int copyCount;
		if (count < 0) {
			copyCount = array.count() - start;
		} else {
			copyCount = count;
			if (start + copyCount > array.count()) {
				throw new IndexOutOfBoundsException();
			}
		}
		return privateCopy(array, copyCount + before + after, start, copyCount, before);
		/*
		udanax-top.st:34166:PrimSpec methodsFor: 'making'!
		{PrimArray} copy: array {PrimArray}
			with: count {Int32 default: -1}
			with: start {Int32 default: Int32Zero}
			with: before {Int32 default: Int32Zero}
			with: after {Int32 default: Int32Zero}
			"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
			
			| copyCount {Int32} |
			count < Int32Zero
				ifTrue: [copyCount _ array count - start]
				ifFalse:
					[copyCount _ count.
					start + copyCount > array count
						ifTrue: [Heaper BLAST: #IndexOutOfBounds]].
			^self privateCopy: array with: copyCount + before + after with: start with: copyCount with: before!
		*/
	}
	
	/**
	 * Make a copy of the array into a larger array.  The array has 'after' slots after the
	 * copied elements.
	 */
	public PrimArray copyGrow(PrimArray array, int after) {
		return copy(array, -1, 0, 0, after);
		/*
		udanax-top.st:34182:PrimSpec methodsFor: 'making'!
		{PrimArray} copyGrow: array {PrimArray} with: after {Int32}
			"Make a copy of the array into a larger array.  The array has 'after' slots after the copied elements."
			
			^self copy: array with: -1 with: Int32Zero with: Int32Zero with: after!
		*/
	}
	
	/**
	 * Support for copy:with:with:with:with:
	 */
	protected abstract PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset);
	/*
	udanax-top.st:34083:PrimSpec methodsFor: 'private: making'!
	{PrimArray} privateCopy: array {PrimArray}
	with: size {Int32 default: -1}
	with: start {Int32 default: Int32Zero}
	with: count {Int32 default: -1}
	with: offset {Int32 default: Int32Zero}
	"Support for copy:with:with:with:with:"
	
	self subclassResponsibility.!
	*/ //	/**
	//	 * Essential. The size of a single element of the array, to be used to allocated space for
	//	 * copyTo/FromBuffer. In the same units as C sizeof ().
	//	 */
	//	public int sizeofElement() {
	//		throw new UnsupportedOperationException();
	//		/*
	//		udanax-top.st:34189:PrimSpec methodsFor: 'accessing'!
	//		{Int32 CLIENT} sizeofElement
	//			"Essential. The size of a single element of the array, to be used to allocated space for copyTo/FromBuffer. In the same units as C sizeof ()."
	//			
	//			self unimplemented.
	//			^Int32Zero "fodder"!
	//		*/
	//	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		/*
		udanax-top.st:34197:PrimSpec methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}
	
	//	public PrimSpec(Rcvr receiver) {
	//		super(receiver);
	//		myClass = receiver.receiveHeaper();
	//		/*
	//		udanax-top.st:34202:PrimSpec methodsFor: 'generated:'!
	//		create.Rcvr: receiver {Rcvr}
	//			super create.Rcvr: receiver.
	//			myClass _ receiver receiveHeaper.!
	//		*/
	//	}
	//
	//	public void sendSelfTo(Xmtr xmtr) {
	//		super.sendSelfTo(xmtr);
	//		xmtr.sendHeaper(myClass);
	//		/*
	//		udanax-top.st:34206:PrimSpec methodsFor: 'generated:'!
	//		{void} sendSelfTo: xmtr {Xmtr}
	//			super sendSelfTo: xmtr.
	//			xmtr sendHeaper: myClass.!
	//		*/
	//	}
	//	/**
	//	 * {PrimArray CLIENT} arrayFromBuffer: count {Int32} with: buffer {void star}
	//	 * {PrimArray CLIENT} arrayWith: value {Heaper}
	//	 * {PrimArray CLIENT} arrayWithThree: value {Heaper} with: other {Heaper} with: another
	//	 * {Heaper}
	//	 * {PrimArray CLIENT} arrayWithTwo: value {Heaper} with: other {Heaper}
	//	 * {Int32 CLIENT} sizeofElement
	//	 */
	//	public static void info() {
	//		/*
	//		udanax-top.st:34320:PrimSpec class methodsFor: 'smalltalk: system'!
	//		info.stProtocol
	//		"{PrimArray CLIENT} arrayFromBuffer: count {Int32} with: buffer {void star}
	//		{PrimArray CLIENT} arrayWith: value {Heaper}
	//		{PrimArray CLIENT} arrayWithThree: value {Heaper} with: other {Heaper} with: another {Heaper}
	//		{PrimArray CLIENT} arrayWithTwo: value {Heaper} with: other {Heaper}
	//		{Int32 CLIENT} sizeofElement
	//		"!
	//		*/
	//	}
}
