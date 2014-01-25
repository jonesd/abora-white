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

import info.dgjones.abora.white.collection.arrays.IEEE32Array;
import info.dgjones.abora.white.collection.arrays.IEEE64Array;
import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.hash.FHash;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Specifies different precisions and representations of floating point numbers.
 */
public class PrimFloatSpec extends PrimSpec {
	protected final int myBitCount;

	//////////////////////////////////////////////
	// Constructors
	
	protected PrimFloatSpec(Class arrayClass, int bitCount) {
		super(arrayClass);
		myBitCount = bitCount;
	}


	//////////////////////////////////////////////
	// Accessing

	/**
	 * Return the total number of bits per value that meets the specification.
	 * 
	 * @return total number of bits per value.
	 */
	public int bitCount() {
		return myBitCount;
	}

	public int actualHashForEqual() {
		//TODO using class rather than category, is that ok? should we use class.name instead?
		return getClass().hashCode() ^ FHash.hashInt(myBitCount);
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof PrimFloatSpec) {
			PrimFloatSpec spec = (PrimFloatSpec) other;
			return myBitCount == spec.bitCount();
		} else {
			return false;
		}
	}

	/**
	 * Make a copy of an array with a different representation size. The arguments are the same
	 * as in PrimArray::copy.
	 */
	public PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset) {
		//TODO Eric Add case for generic floating point--essentially PtrArray of PrimFloat */
		if (this == PrimSpec.iEEE64()) {
			return IEEE64Array.make(size, array, start, count, offset);
		}
		if (this == PrimSpec.iEEE32()) {
			return IEEE32Array.make(size, array, start, count, offset);
		}
		throw new IllegalStateException("BadPrimSpec");
		/*
		udanax-top.st:34368:PrimFloatSpec methodsFor: 'private: making'!
		{PrimArray} privateCopy: array {PrimArray}
			with: size {Int32 default: -1}
			with: start {Int32 default: Int32Zero}
			with: count {Int32 default: -1}
			with: offset {Int32 default: Int32Zero}
			"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
			
			[Eric thingToDo. "Add case for generic floating point--essentially PtrArray of PrimFloat"
			self == (PrimSpec iEEE64 basicCast: Heaper star) ifTrue: [^IEEE64Array make: size with: array with: start with: count with: offset].
			self == (PrimSpec iEEE32 basicCast: Heaper star) ifTrue: [^IEEE32Array make: size with: array with: start with: count with: offset].
			Heaper BLAST: #BadPrimSpec] translateOnly.
			[^myClass create: size with: start with: count with: offset] smalltalkOnly.
			^ NULL "compiler fodder"!
		*/
	}

//	/**
//	 * Make an array initialized to zero values
//	 */
//	public PrimArray array(int count) {
//		// TODO Eric Add case for generic floating point--essentially PtrArray of PrimFloat */
//		if (this == PrimSpec.iEEE64()) {
//			return IEEE64Array.make(count);
//		}
//		if (this == PrimSpec.iEEE32()) {
//			return IEEE32Array.make(count);
//		}
//		throw new IllegalStateException("BadPrimSpec");
//		/*
//		udanax-top.st:34384:PrimFloatSpec methodsFor: 'making'!
//		{PrimArray} array: count {Int32 default: Int32Zero}
//			"Make an array initialized to zero values"
//			[Eric thingToDo. "Add case for generic floating point--essentially PtrArray of PrimFloat"
//			self == (PrimSpec iEEE64 basicCast: Heaper star) ifTrue: [^IEEE64Array make: count].
//			self == (PrimSpec iEEE32 basicCast: Heaper star) ifTrue: [^IEEE32Array make: count].
//			Heaper BLAST: #BadPrimSpec] translateOnly.
//			[^myClass create: count] smalltalkOnly.
//			^ NULL "compiler fodder"!
//		*/
//	}

	/**
	 * Make an array with the values at the given address
	 */
	public PrimArray arrayFromBuffer(Object buffer) {
		/* Generic case of unspecified size can't be handled here. */
		if (this == PrimSpec.iEEE64()) {
			return IEEE64Array.make((double[])buffer);
		}
		if (this == PrimSpec.iEEE32()) {
			return IEEE32Array.make((float[])buffer);
		}
		throw new IllegalStateException("BadPrimSpec");
		/*
		udanax-top.st:34394:PrimFloatSpec methodsFor: 'making'!
		{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
			"Make an array with the values at the given address"
			
			["Generic case of unspecified size can't be handled here."
			self == (PrimSpec iEEE64 basicCast: Heaper star) ifTrue: [^IEEE64Array make: count with: buffer].
			self == (PrimSpec iEEE32 basicCast: Heaper star) ifTrue: [^IEEE32Array make: count with: buffer].
			Heaper BLAST: #BadPrimSpec] translateOnly.
			[^myClass create: count with: buffer] smalltalkOnly.
			^ NULL "compiler fodder"!
		*/
	}

//	/**
//	 * A boxed floating point value from a large precision number
//	 */
//	public PrimFloatValue preciseValue(IEEE128 number) {
//		/* myBitCount = 32 ifTrue: [^PrimIEEE32 make: self with: number].
//			myBitCount = 64 ifTrue: [^PrimIEEE64 make: self with: number] */
//		throw new UnsupportedOperationException();
//		/*
//		udanax-top.st:34404:PrimFloatSpec methodsFor: 'making'!
//		{PrimFloatValue} preciseValue: number {IEEE128}
//			"A boxed floating point value from a large precision number"
//			
//			"myBitCount = 32 ifTrue: [^PrimIEEE32 make: self with: number].
//			myBitCount = 64 ifTrue: [^PrimIEEE64 make: self with: number]"
//			self unimplemented.
//			^NULL "fodder"!
//		*/
//	}

	/**
	 * A boxed floating point value
	 */
	public PrimFloatValue value(double number) {
		if (myBitCount == 32) {
			return IEEE32Value.make((float)number);
		}
		if (myBitCount == 64) {
			return IEEE64Value.make(number);
		}
		//TODO should this fail instead?
		return null;
		/*
		udanax-top.st:34412:PrimFloatSpec methodsFor: 'making'!
		{PrimFloatValue} value: number {IEEE64}
			"A boxed floating point value"
			
			myBitCount = 32 ifTrue: [^PrimIEEE32 make: number].
			myBitCount = 64 ifTrue: [^PrimIEEE64 make: number].
			^NULL "fodder"!
		*/
	}

	//		public int precision() {
	//			passe()
	//			/* bitCount */;
	//			/*
	//			udanax-top.st:34421:PrimFloatSpec methodsFor: 'smalltalk: passe'!
	//			{Int32} precision
	//				self passe "bitCount"!
	//			*/
	//		}

	//		public PrimFloatSpec(Rcvr receiver) {
	//			super(receiver);
	//			myBitCount = receiver.receiveInt32();
	//			/*
	//			udanax-top.st:34427:PrimFloatSpec methodsFor: 'generated:'!
	//			create.Rcvr: receiver {Rcvr}
	//				super create.Rcvr: receiver.
	//				myBitCount _ receiver receiveInt32.!
	//			*/
	//		}

	//		public void sendSelfTo(Xmtr xmtr) {
	//			super.sendSelfTo(xmtr);
	//			xmtr.sendInt32(myBitCount);
	//			/*
	//			udanax-top.st:34431:PrimFloatSpec methodsFor: 'generated:'!
	//			{void} sendSelfTo: xmtr {Xmtr}
	//				super sendSelfTo: xmtr.
	//				xmtr sendInt32: myBitCount.!
	//			*/
	//		}

	//		/**
	//		 * {PrimFloat CLIENT} preciseValue: number {IEEE128}
	//		 * {Int32 CLIENT} precision
	//		 * {PrimFloat CLIENT} value: number {IEEE64}
	//		 */
	//		public static void info() {
	//			/*
	//			udanax-top.st:34444:PrimFloatSpec class methodsFor: 'smalltalk: system'!
	//			info.stProtocol
	//			"{PrimFloat CLIENT} preciseValue: number {IEEE128}
	//			{Int32 CLIENT} precision
	//			{PrimFloat CLIENT} value: number {IEEE64}
	//			"!
	//			*/
	//		}
}
