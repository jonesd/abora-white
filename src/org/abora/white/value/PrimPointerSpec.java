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

import org.abora.white.collection.arrays.PrimArray;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.arrays.SharedPtrArray;
import org.abora.white.xpp.basic.Heaper;

/**
 * Describes a kind of primitive pointer array
 */
public class PrimPointerSpec extends PrimSpec {

	//////////////////////////////////////////////
	// Constructors

	protected PrimPointerSpec(Class arrayClass) {
		super(arrayClass);
	}

	public int actualHashForEqual() {
		return getClass().hashCode() ^ arrayClass().hashCode();
	}

	public boolean isEqual(Heaper other) {
		return (other instanceof PrimPointerSpec) && (arrayClass() == ((PrimPointerSpec) other).arrayClass());
		/*
		udanax-top.st:34643:PrimPointerSpec methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			^(other isKindOf: PrimPointerSpec) and: [self arrayClass == (other cast: PrimPointerSpec) arrayClass]!
		*/
	}

	/**
	 * Make a copy of an array with a different representation size. The arguments are the same
	 * as in PrimArray::copy.
	 */
	protected PrimArray privateCopy(PrimArray array, int size, int start, int count, int offset) {
		if (this == PrimSpec.pointer()) {
			return PtrArray.make(size, array, start, count, offset);
		}
		if (this == PrimSpec.sharedPointer()) {
			return SharedPtrArray.make(size, array, start, count, offset);
		}
		throw new IllegalStateException("BadPrimSpec");
		/*
		udanax-top.st:34649:PrimPointerSpec methodsFor: 'private: making'!
		{PrimArray} privateCopy: array {PrimArray}
			with: size {Int32 default: -1}
			with: start {Int32 default: Int32Zero}
			with: count {Int32 default: -1}
			with: offset {Int32 default: Int32Zero}
			"Make a copy of an array with a different representation size. The arguments are the same as in PrimArray::copy."
			
			[self == (PrimSpec pointer basicCast: Heaper star) ifTrue: [^PtrArray make: size with: array with: start with: count with: offset].
			self == (PrimSpec sharedPointer basicCast: Heaper star) ifTrue: [^SharedPtrArray make: size with: array with: start with: count with: offset].
			Heaper BLAST: #BadPrimSpec] translateOnly.
			[^myClass create: size with: start with: count with: offset] smalltalkOnly.
			^ NULL "compiler fodder"!
		*/
	}

//	/**
//	 * Make an array initialized to null values
//	 */
//	public PrimArray array(int count) {
//		if (this == PrimSpec.pointer()) {
//			return PtrArray.nulls(count);
//		}
//		if (this == PrimSpec.sharedPointer()) {
//			return SharedPtrArray.make(count);
//		}
//		throw new IllegalStateException("BadPrimSpec");
//		/*
//		udanax-top.st:34670:PrimPointerSpec methodsFor: 'making'!
//		{PrimArray} array: count {Int32 default: Int32Zero}
//			"Make an array initialized to null values"
//			[self == (PrimSpec pointer basicCast: Heaper star) ifTrue: [^PtrArray nulls: count].
//			self == (PrimSpec sharedPointer basicCast: Heaper star) ifTrue: [^SharedPtrArray make: count].
//			Heaper BLAST: #BadPrimSpec] translateOnly.
//			[^myClass create: count] smalltalkOnly.
//			^ NULL "compiler fodder"!
//		*/
//	}

	/**
	 * Make an array with the values at the given address
	 */
	public PrimArray arrayFromBuffer(Object buffer) {
		throw new UnsupportedOperationException();
//		if (this == PrimSpec.pointer()) {
//			return PtrArray.make(buffer);
//		}
//		if (this == PrimSpec.sharedPointer()) {
//			return SharedPtrArray.make(buffer);
//		}
//		throw new IllegalStateException("BadPrimSpec");
		/*
		udanax-top.st:34679:PrimPointerSpec methodsFor: 'making'!
		{PrimArray} arrayFromBuffer: count {Int32} with: buffer {void star}
			"Make an array with the values at the given address"
			[self == (PrimSpec pointer basicCast: Heaper star) ifTrue: [^PtrArray make: count with: buffer].
			self == (PrimSpec sharedPointer basicCast: Heaper star) ifTrue: [^SharedPtrArray make: count with: buffer].
			Heaper BLAST: #BadPrimSpec] translateOnly.
			[^myClass create: count with: buffer] smalltalkOnly.
			^ NULL "compiler fodder"!
		*/
	}

	//	public PrimPointerSpec(Rcvr receiver) {
	//		super(receiver);
	//		/*
	//		udanax-top.st:34690:PrimPointerSpec methodsFor: 'generated:'!
	//		create.Rcvr: receiver {Rcvr}
	//			super create.Rcvr: receiver.!
	//		*/
	//	}

	//	public void sendSelfTo(Xmtr xmtr) {
	//		super.sendSelfTo(xmtr);
	//		/*
	//		udanax-top.st:34693:PrimPointerSpec methodsFor: 'generated:'!
	//		{void} sendSelfTo: xmtr {Xmtr}
	//			super sendSelfTo: xmtr.!
	//		*/
	//	}
}
