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
package info.dgjones.abora.white.value;

import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.arrays.SharedPtrArray;
import info.dgjones.abora.white.xpp.basic.Heaper;

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
