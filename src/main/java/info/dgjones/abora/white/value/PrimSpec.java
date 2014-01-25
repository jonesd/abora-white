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
package info.dgjones.abora.white.value;

import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;

import info.dgjones.abora.white.collection.arrays.IEEE32Array;
import info.dgjones.abora.white.collection.arrays.IEEE64Array;
import info.dgjones.abora.white.collection.arrays.Int16Array;
import info.dgjones.abora.white.collection.arrays.Int32Array;
import info.dgjones.abora.white.collection.arrays.Int64Array;
import info.dgjones.abora.white.collection.arrays.Int8Array;
import info.dgjones.abora.white.collection.arrays.IntegerVarArray;
import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.arrays.SharedPtrArray;
import info.dgjones.abora.white.collection.arrays.UInt16Array;
import info.dgjones.abora.white.collection.arrays.UInt32Array;
import info.dgjones.abora.white.collection.arrays.UInt8Array;
import info.dgjones.abora.white.xpp.basic.Heaper;

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
	}
	
	//////////////////////////////////////////////
	// Array Factory Methods
	
	/**
	 * Return an array suitable for holding the specified <code>count</code>
	 * number of elements that match <code>this</code> specification. Elements
	 * should be initialized to some reasonable zero/null value.
	 * 
	 * @param count number of elements this will be able to hold
	 * @return the array 
	 */
	public PrimArray array(int count) {
		//TODO Udanax-Gold has subclasses overriding an abstract array(count)
		// and then a switch on the spec instance to know how to construct
		// the array. Is this ok in comparison? Performance?
		try {
			Method method = arrayClass.getDeclaredMethod("make", new Class[] { Integer.TYPE });
			return (PrimArray) method.invoke(null, new Object[]{new Integer(count)});
		} catch (NoSuchMethodException e) {
			//TODO what should be the strategy for catching these kinds
			// of (probably code) problems?
			throw new IllegalStateException(e.toString());
		} catch (IllegalAccessException e) {
			throw new IllegalStateException(e.toString());
		} catch (InvocationTargetException e) {
			throw new IllegalStateException(e.toString());
		}
	}

	/**
	 * Return an empty array suitable for holding elements that match <code>this</code> specification.
	 * 
	 * @return the array 
	 */
	public PrimArray array() {
		return array(0);
	}
	
	/**
	 * Return an array holding a copy of the given elements of <tt>buffer</tt>
	 * suitable for <tt>this</tt> spec.
	 * 
	 * @param buffer Java array containing elements meeting <tt>this</tt> specification.
	 * @return the array 
	 */
	public abstract PrimArray arrayFromBuffer(Object buffer);
	
	/**
	 * Return an array holding the single element containing the
	 * given <code>value</code>. 
	 * 
	 * @param heaper value to store in array
	 * @return the array 
	 */
	public PrimArray arrayWith(Heaper value) {
		PrimArray result = array(1);
		result.storeValue(0, value);
		return result;
	}

	/**
	 * Return an array holding two elements containing the
	 * given values in the same order. 
	 * 
	 * @param value first value to store in array.
	 * @param other second value to store in array.
	 * @return the array 
	 */
	public PrimArray arrayWithTwo(Heaper value, Heaper other) {
		PrimArray result = array(2);
		result.storeValue(0, value);
		result.storeValue(1, other);
		return result;
	}
	
	/**
	 * Return an array holding three elements containing the
	 * given values in the same order. 
	 * 
	 * @param value first value to store in array.
	 * @param other second value to store in array.
	 * @param another third value to store in array.
	 * @return the array 
	 */
	public PrimArray arrayWithThree(Heaper value, Heaper other, Heaper another) {
		PrimArray result = array(3);
		result.storeValue(0, value);
		result.storeValue(1, other);
		result.storeValue(2, another);
		return result;
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
