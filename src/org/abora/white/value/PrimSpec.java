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
import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.IntegerVarArray;
import org.abora.white.collection.arrays.PrimArray;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.arrays.SharedPtrArray;
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
	
	protected final static PrimFloatSpec TheIEEE32Spec = new PrimFloatSpec(IEEE32Array.class, 32);
	protected final static PrimFloatSpec TheIEEE64Spec = new PrimFloatSpec(IEEE64Array.class, 64);
	protected final static PrimIntegerSpec TheInt32Spec = new PrimIntegerSpec(Int32Array.class, 32, true);;
	protected final static PrimIntegerSpec TheIntegerVarSpec = new PrimIntegerSpec(IntegerVarArray.class, -1, true);;
	protected final static PrimPointerSpec ThePtrSpec = new PrimPointerSpec(PtrArray.class);;
	protected final static PrimPointerSpec TheSharedPtrSpec = new PrimPointerSpec(SharedPtrArray.class);;
	protected final static PrimIntegerSpec TheUInt32Spec = new PrimIntegerSpec(UInt32Array.class, 32, false);;
	protected final static PrimIntegerSpec TheUInt8Spec = new PrimIntegerSpec(UInt8Array.class, 8, false);;

	/*
	udanax-top.st:34219:PrimSpec class methodsFor: 'private: init'!
	{void} initSpecs
		"moved from initTime because MS C++/NT does not like large initTimes"
		TheUInt8Spec := PrimIntegerSpec create: UInt8Array with: 8 with: false.
		TheUInt32Spec := PrimIntegerSpec create: UInt32Array with: 32 with: false.
		TheInt32Spec := PrimIntegerSpec create: Int32Array with: 32 with: true.
		TheIntegerVarSpec := PrimIntegerSpec create: IntegerVarArray with: -1 with: true.
		TheIEEE32Spec := PrimFloatSpec create: IEEE32Array with: 32.
		TheIEEE64Spec := PrimFloatSpec create: IEEE64Array with: 64.
		ThePtrSpec := PrimPointerSpec create: PtrArray.
		TheSharedPtrSpec := PrimPointerSpec create: SharedPtrArray.!
	*/

	/*
	udanax-top.st:34064:
	Heaper subclass: #PrimSpec
		instanceVariableNames: 'myClass {Category}'
		classVariableNames: '
			TheIEEE32Spec {PrimFloatSpec} 
			TheIEEE64Spec {PrimFloatSpec} 
			TheInt32Spec {PrimIntegerSpec} 
			TheIntegerVarSpec {PrimIntegerSpec} 
			ThePtrSpec {PrimPointerSpec} 
			TheSharedPtrSpec {PrimPointerSpec} 
			TheUInt32Spec {PrimIntegerSpec} 
			TheUInt8Spec {PrimIntegerSpec} '
		poolDictionaries: ''
		category: 'X++ PrimArrays'!
	*/
	/*
	udanax-top.st:34076:
	PrimSpec comment:
	'A specification of a kind of primitive data type which can be stored in PrimArrays. It gives you protocol for creating and copying PrimArrays. The class and characteristics of this object determine what kind of things are stored there, and how much precision they have.'!
	*/
	/*
	udanax-top.st:34078:
	(PrimSpec getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #(COPY xpp ); add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:34211:
	PrimSpec class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:34214:
	(PrimSpec getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #(COPY xpp ); add: #DEFERRED; yourself)!
	*/

	//////////////////////////////////////////////
	// Constructors

	protected PrimSpec(Class arrayClass) {
		super();
		this.arrayClass = arrayClass;
		/*
		udanax-top.st:34123:PrimSpec methodsFor: 'protected: create'!
		create: primClass {Category}
			super create.
			myClass _ primClass.!
		*/
	}


	//////////////////////////////////////////////
	// Static Factory Methods
	
	public static PrimFloatSpec iEEE32() {
		return TheIEEE32Spec;
		/*
		udanax-top.st:34250:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimFloatSpec INLINE} iEEE32
			^TheIEEE32Spec!
		*/
	}

	public static PrimFloatSpec iEEE64() {
		return TheIEEE64Spec;
		/*
		udanax-top.st:34254:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimFloatSpec INLINE} iEEE64
			^TheIEEE64Spec!
		*/
	}

	public static PrimFloatSpec iEEE(int precision) {
		if (precision == 32) {
			return TheIEEE32Spec;
		}
		if (precision == 64) {
			return TheIEEE64Spec;
		}
		throw new UnsupportedOperationException();
		/*
		udanax-top.st:34258:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimFloatSpec} iEEE: precision {Int32}
			
			precision = 32 ifTrue: [^TheIEEE32Spec].
			precision = 64 ifTrue: [^TheIEEE64Spec].
			self unimplemented.
			^NULL!
		*/
	}

	public static PrimIntegerSpec int32() {
		return TheInt32Spec;
		/*
		udanax-top.st:34265:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimIntegerSpec INLINE} int32
			^TheInt32Spec!
		*/
	}

	public static PrimIntegerSpec integerVar() {
		return TheIntegerVarSpec;
		/*
		udanax-top.st:34269:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimIntegerSpec INLINE} integerVar
			^TheIntegerVarSpec!
		*/
	}

	/**
	 * A spec for pointers to object
	 */
	public static PrimPointerSpec pointer() {
		return ThePtrSpec;
		/*
		udanax-top.st:34273:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimPointerSpec INLINE} pointer
			"A spec for pointers to object"
			^ThePtrSpec!
		*/
	}

	public static PrimPointerSpec sharedPointer() {
		return TheSharedPtrSpec;
		/*
		udanax-top.st:34278:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimPointerSpec INLINE} sharedPointer
			^TheSharedPtrSpec!
		*/
	}

	public static PrimIntegerSpec signedInteger(int bitCount) {
		if (bitCount == 32) {
			return TheInt32Spec;
		}
		throw new UnsupportedOperationException();
		/*
		udanax-top.st:34282:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimIntegerSpec} signedInteger: bitCount {Int32}
			bitCount = 32 ifTrue: [^TheInt32Spec].
			self unimplemented.
			^NULL!
		*/
	}

	/**
	 * The least demanding spec that will hold the given value
	 */
	public static PrimIntegerSpec toHold(IntegerValue value) {
		throw new UnsupportedOperationException();
//		if (value < IntegerValue.zero()) {
//			if (value < Int32Min) {
//				return integerVar();
//			} else {
//				return int32();
//			}
//		} else {
//			if (value <= Int32Max) {
//				if (value <= UInt8Max) {
//					return uInt8();
//				} else {
//					return int32();
//				}
//			} else {
//				if (value <= UInt32Max) {
//					return uInt32();
//				} else {
//					return integerVar();
//				}
//			}
//		}
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

	public static PrimIntegerSpec uInt32() {
		return TheUInt32Spec;
		/*
		udanax-top.st:34303:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimIntegerSpec INLINE} uInt32
			^TheUInt32Spec!
		*/
	}

	public static PrimIntegerSpec uInt8() {
		return TheUInt8Spec;
		/*
		udanax-top.st:34307:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimIntegerSpec INLINE} uInt8
			^TheUInt8Spec!
		*/
	}

	public static PrimIntegerSpec unsignedInteger(int bitCount) {
		if (bitCount == 32) {
			return TheUInt32Spec;
		}
		if (bitCount == 8) {
			return TheUInt8Spec;
		}
		throw new UnsupportedOperationException();
		/*
		udanax-top.st:34311:PrimSpec class methodsFor: 'pseudo constructors'!
		{PrimIntegerSpec} unsignedInteger: bitCount {Int32}
			bitCount = 32 ifTrue: [^TheUInt32Spec].
			bitCount = 8 ifTrue: [^TheUInt8Spec].
			self unimplemented.
			^NULL!
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
	*/

	public PrimArray array() {
		return array(0);
		/*
		udanax-top.st:34094:PrimSpec methodsFor: 'smalltalk: defaults'!
		{PrimArray} array
			^self array: 0!
		*/
	}

	public PrimArray copy(PrimArray array) {
		return copy(array, -1, 0, 0, 0);
		/*
		udanax-top.st:34098:PrimSpec methodsFor: 'smalltalk: defaults'!
		{PrimArray} copy: array {PrimArray}
			^self copy: array with: -1 with: 0 with: 0 with: 0!
		*/
	}

	public PrimArray copy(PrimArray array, int count) {
		return copy(array, count, 0, 0, 0);
		/*
		udanax-top.st:34101:PrimSpec methodsFor: 'smalltalk: defaults'!
		{PrimArray} copy: array {PrimArray}
			with: count {Int32 default: -1}
			^self copy: array with: count with: 0 with: 0 with: 0!
		*/
	}

	public PrimArray copy(PrimArray array, int count, int start) {
		return copy(array, count, start, 0, 0);
		/*
		udanax-top.st:34105:PrimSpec methodsFor: 'smalltalk: defaults'!
		{PrimArray} copy: array {PrimArray}
			with: count {Int32 default: -1}
			with: start {Int32 default: Int32Zero}
			^self copy: array with: count with: start with: 0 with: 0!
		*/
	}

	public PrimArray copy(PrimArray array, int count, int start, int before) {
		return copy(array, count, start, before, 0);
		/*
		udanax-top.st:34110:PrimSpec methodsFor: 'smalltalk: defaults'!
		{PrimArray} copy: array {PrimArray}
			with: count {Int32 default: -1}
			with: start {Int32 default: Int32Zero}
			with: before {Int32 default: Int32Zero}
			^self copy: array with: count with: start with: before with: 0!
		*/
	}

	public Class arrayClass() {
		return arrayClass;
		/*
		udanax-top.st:34118:PrimSpec methodsFor: 'protected:'!
		{Category INLINE} arrayClass
			^myClass!
		*/
	}

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
		PrimArray result;
		result = array(1);
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
		PrimArray result;
		result = array(3);
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
		PrimArray result;
		result = array(2);
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

//	/**
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
		throw new UnsupportedOperationException();
//		return Heaper.takeOop();
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
