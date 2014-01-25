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
package info.dgjones.abora.white.spaces.integers;

import java.io.PrintWriter;

import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Because of the constraints of C++, we have two very different types representing integers
 * in our system.
 * XuInteger is the boxed representation which must be used in any context which only knows
 * that it is dealing with a Position.  XuInteger is a Heaper with all that implies.
 * Specifically, one can take advantage of all the advantages of polymorphism (leading to
 * uses by code that only knows it is dealing with a Position), but at the cost of
 * representing each value by a heap allocated object to which pointers are passed.  Such a
 * representation is referred to as "boxed" because the pointer combined with the storage
 * structure overhead of maintaining a heap allocated object constitutes a "box" between the
 * user of the data (the guy holding onto the pointer), and the actual data (which is inside
 * the Heaper).
 * In contrast, IntegerValue is the efficient, unboxed representation of an integer.
 * (actually, it is only unboxed so long as it fits within some size limit such as 32 bits.
 * Those IntegerValues that exceed this limit pay their own boxing cost to store their
 * representation on the heap.  This need not concern us here.)  See "The Var vs Heaper
 * distinction" and IntegerValue.  When we know that we are dealing specifically with an
 * integer, we`d like to be able to stick with IntegerValues without having to convert them to
 * XuIntegers.  However, we`d like to be able to do everything that we could normally do if
 * we had an XuInteger.
 * For this purpose, many messages (such as Position * Dsp::of(Position*)) have an additional
 * overloading (IntegerValue Dsp::of(IntegerValue)) whose semantics is defined in terms of
 * converting the argument to an XuInteger, applying the original operation, and converting
 * the result (which is asserted to be an XuInteger) back to an IntegerValue.  Dsp even
 * provides a default implementation to do exactly that.  However, if we actually rely on
 * this default implementation then we are defeating the whole purpose of avoiding boxing
 * overhead.  Instead, IntegerDsp overrides this to provide an efficient implementation.
 * Any particular case may at the moment simply be relying on the default.  The point is to
 * get interfaces defined early which allow efficiency tuning to proceed in a modular fashion
 * later.  Should any particular reliance on the default actually prove to be an efficiency
 * issue, we will deal with it then.
 */
public class IntegerPos extends Position {
	protected IntegerValue myValue;
	protected static IntegerPos TheZero;
	/*
	udanax-top.st:31874:
	Position subclass: #IntegerPos
		instanceVariableNames: 'myValue {IntegerValue}'
		classVariableNames: 'TheZero {IntegerPos} '
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Integers'!
	*/
	/*
	udanax-top.st:31878:
	IntegerPos comment:
	'Because of the constraints of C++, we have two very different types representing integers in our system.  
		
		XuInteger is the boxed representation which must be used in any context which only knows that it is dealing with a Position.  XuInteger is a Heaper with all that implies.  Specifically, one can take advantage of all the advantages of polymorphism (leading to uses by code that only knows it is dealing with a Position), but at the cost of representing each value by a heap allocated object to which pointers are passed.  Such a representation is referred to as "boxed" because the pointer combined with the storage structure overhead of maintaining a heap allocated object constitutes a "box" between the user of the data (the guy holding onto the pointer), and the actual data (which is inside the Heaper).
		
		In contrast, IntegerValue is the efficient, unboxed representation of an integer.  (actually, it is only unboxed so long as it fits within some size limit such as 32 bits.  Those IntegerValues that exceed this limit pay their own boxing cost to store their representation on the heap.  This need not concern us here.)  See "The Var vs Heaper distinction" and IntegerValue.  When we know that we are dealing specifically with an integer, we`d like to be able to stick with IntegerValues without having to convert them to XuIntegers.  However, we`d like to be able to do everything that we could normally do if we had an XuInteger.
		
		For this purpose, many messages (such as Position * Dsp::of(Position*)) have an additional overloading (IntegerValue Dsp::of(IntegerValue)) whose semantics is defined in terms of converting the argument to an XuInteger, applying the original operation, and converting the result (which is asserted to be an XuInteger) back to an IntegerValue.  Dsp even provides a default implementation to do exactly that.  However, if we actually rely on this default implementation then we are defeating the whole purpose of avoiding boxing overhead.  Instead, IntegerDsp overrides this to provide an efficient implementation.  
		
		Any particular case may at the moment simply be relying on the default.  The point is to get interfaces defined early which allow efficiency tuning to proceed in a modular fashion later.  Should any particular reliance on the default actually prove to be an efficiency issue, we will deal with it then.'!
	*/
	/*
	udanax-top.st:31888:
	(IntegerPos getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:31971:
	IntegerPos class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:31974:
	(IntegerPos getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #ON.CLIENT; add: #COPY; yourself)!
	*/

	/**
	 * This must use an external function so other parts of the system can
	 * compute the hash from an integerVar without boxing.
	 */
	public int actualHashForEqual() {
		/* Open-code in smalltalk because we don't have inlines. */
		/* NOTE:  Do NOT change this without also changing the implementation of integerHash!!!!!!. */
		/* >>> smalltalkOnly */
		return ((myValue.asInt32() * 99991) ^ 98953);
		//		return (((myValue.DOTasLong() * 99991).lo3bytes()) ^ 98953)
		/* bitShiftRight: 6 */
		/* <<< smalltalkOnly */
		//		{
		//			return IntegerPos.integerHash(myValue);
		//		}
		//		translateOnly;
		/*
		udanax-top.st:31893:IntegerPos methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			"This must use an external function so other parts of the system can
			 compute the hash from an integerVar without boxing."
			 
			"Open-code in smalltalk because we don't have inlines."
			"NOTE:  Do NOT change this without also changing the implementation of integerHash!!!!!!."
			[^(((myValue DOTasLong * 99991) lo3bytes) bitXor: 98953) "bitShiftRight: 6"] smalltalkOnly.
			[^IntegerPos integerHash: myValue] translateOnly!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof IntegerPos) {
			IntegerPos xui = (IntegerPos) other;
			return xui.asIntegerVar().isEqual(myValue);
		} else {
			return false;
		}
		/*
		udanax-top.st:31902:IntegerPos methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper} 
			other
				cast: IntegerPos into: [:xui |
					^xui asIntegerValue = myValue]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	/**
	 * Just the full ordering you'd expect on integers
	 */
	public boolean isGE(Position other) {
		if (other instanceof IntegerPos) {
			IntegerPos xui = (IntegerPos) other;
			return myValue.isGE(xui.asIntegerVar());
		} else {
			throw new AboraRuntimeException(AboraRuntimeException.CANT_MIX_COORDINATE_SPACES);
		}
		/*
		udanax-top.st:31910:IntegerPos methodsFor: 'testing'!
		{BooleanVar} isGE: other {Position}
			"Just the full ordering you'd expect on integers"
			
			other 
				cast: IntegerPos into: [:xui |
					^myValue >= xui asIntegerValue]
				others: [Heaper BLAST: #CantMixCoordinateSpaces].
			^ false "compiler fodder"!
		*/
	}

	/**
	 * Unboxed version as an integer.  See class comment
	 */
	public int asInt32() {
		return myValue.asInt32();
		/*
		udanax-top.st:31921:IntegerPos methodsFor: 'accessing'!
		{Int32 INLINE} asInt32
			"Unboxed version as an integer.  See class comment"
			
			^myValue DOTasLong!
		*/
	}

	/**
	 * Essential.  Unboxed version.  See class comment
	 */
	public IntegerValue asIntegerVar() {
		return myValue;
		/*
		udanax-top.st:31926:IntegerPos methodsFor: 'accessing'!
		{IntegerValue INLINE} asIntegerValue
			"Essential.  Unboxed version.  See class comment"
			
			^myValue!
		*/
	}

	public XnRegion asRegion() {
		return IntegerRegion.make(asIntegerVar());
		/*
		udanax-top.st:31931:IntegerPos methodsFor: 'accessing'!
		{XnRegion} asRegion
			^IntegerRegion make: self asIntegerValue!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:31935:IntegerPos methodsFor: 'accessing'!
		{CoordinateSpace INLINE} coordinateSpace
			^ IntegerSpace make!
		*/
	}

	/**
	 * Essential.  Unboxed version.  See class comment
	 */
	public IntegerValue value() {
		return myValue;
		/*
		udanax-top.st:31938:IntegerPos methodsFor: 'accessing'!
		{IntegerValue CLIENT INLINE} value
			"Essential.  Unboxed version.  See class comment"
			
			^myValue!
		*/
	}

	//	public void basicCast(Object someClass) {
	//		if (someClass == Character) {
	//			return Character.value(myValue);
	//		} else {
	//			return this;
	//		}
	//		/*
	//		udanax-top.st:31945:IntegerPos methodsFor: 'smalltalk: private:'!
	//		basicCast: someClass
	//			someClass == Character ifTrue: [^ Character value: myValue]
	//				ifFalse: [^self]!
	//		*/
	//	}

	public void printOn(PrintWriter oo) {
		oo.print("I(");
		oo.print(myValue);
		oo.print(")");
		/*
		udanax-top.st:31951:IntegerPos methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << 'I(' << myValue << ')'!
		*/
	}

	public IntegerPos(IntegerValue newValue) {
		super();
		myValue = newValue;
		/*
		udanax-top.st:31956:IntegerPos methodsFor: 'protected: creation'!
		create: newValue {IntegerValue}
			super create.
			myValue _ newValue!
		*/
	}

	public IntegerPos(Rcvr receiver) {
		super(receiver);
		myValue = receiver.receiveIntegerVar();
		/*
		udanax-top.st:31962:IntegerPos methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myValue _ receiver receiveIntegerValue.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendIntegerVar(myValue);
		/*
		udanax-top.st:31966:IntegerPos methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendIntegerValue: myValue.!
		*/
	}

	/**
	 * Box an integer. See XuInteger class comment. you can also create an
	 * integer in smalltalk by sending the integer message to a Smalltalk integer
	 */
	public static IntegerPos make(IntegerValue newValue) {
		return new IntegerPos(newValue);
		/*
		udanax-top.st:31979:IntegerPos class methodsFor: 'pseudo constructors'!
		{IntegerPos INLINE} make: newValue {IntegerValue} 
			"Box an integer. See XuInteger class comment. you can also create an 
			integer in smalltalk by sending the integer message to a Smalltalk integer"
			^IntegerPos create: newValue!
		*/
	}

	public static IntegerPos make(long newValue) {
		return IntegerPos.make(IntegerValue.make(newValue));
	}

	/**
	 * Box an integer. See XuInteger class comment. you can also create an
	 * integer in smalltalk by sending the integer message to a Smalltalk integer.
	 * This should return the canonical zero eventually.
	 */
	public static IntegerPos zero() {
		return IntegerPos.make(IntegerValue.zero());
		/*
		udanax-top.st:31985:IntegerPos class methodsFor: 'pseudo constructors'!
		{IntegerPos INLINE} zero
			"Box an integer. See XuInteger class comment. you can also create an 
			integer in smalltalk by sending the integer message to a Smalltalk integer.
			This should return the canonical zero eventually."
			^IntegerPos make: IntegerValueZero!
		*/
	}

	//	public static void IntegerValue(Object number) {
	//		return number;
	//		/*
	//		udanax-top.st:31994:IntegerPos class methodsFor: 'smalltalk: smalltalk pseudoconstructors'!
	//		IntegerValue: number
	//			^ number!
	//		*/
	//	}

	/**
	 * NOTE:  Do NOT change this without also changing the implementation of hashForEqual in
	 * XuInteger!!!!!!.
	 */
	public static int integerHash(IntegerValue value) {
		/* >>> smalltalkOnly */
		return ((value.asInt32() * 99991) ^ 98953)
		//		return (((value * 99991).lo3bytes()) ^ 98953)
		/* bitShiftRight: 6 */;
		/* <<< smalltalkOnly */
//		return (((value * 99991).DOTasLong() & 16777215) ^ 98953)
		/* bitShiftRight: 6 */
		//		}
		//		translateOnly;
		/*
		udanax-top.st:31999:IntegerPos class methodsFor: 'hash computing'!
		{UInt32 INLINE} integerHash: value {IntegerValue}
			"NOTE:  Do NOT change this without also changing the implementation of hashForEqual in XuInteger!!!!!!."
			[^(((value * 99991) lo3bytes) bitXor: 98953) "bitShiftRight: 6"] smalltalkOnly.
			[^(((value * 99991) DOTasLong bitAnd: 16777215) bitXor: 98953) "bitShiftRight: 6"] translateOnly.!
		*/
	}

	/**
	 * {IntegerValue CLIENT INLINE} value
	 */
	public static void info() {
		/*
		udanax-top.st:32007:IntegerPos class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{IntegerValue CLIENT INLINE} value
		"!
		*/
	}

	public static String exportName() {
		return "Integer";
		/*
		udanax-top.st:32013:IntegerPos class methodsFor: 'smalltalk: promise'!
		exportName
			^'Integer'!
		*/
	}
}
