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
package info.dgjones.abora.white.tumbler;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.PrimFloatValue;
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Represents some real number exactly.  Not all real numbers can be exactly represented.
 * See class comment in RealSpace.
 */
public abstract class RealPos extends Position {
	/*
	udanax-top.st:32016:
	Position subclass: #RealPos
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:32020:
	RealPos comment:
	'Represents some real number exactly.  Not all real numbers can be exactly represented.  See class comment in RealSpace.'!
	*/
	/*
	udanax-top.st:32022:
	(RealPos getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:32113:
	RealPos class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:32116:
	(RealPos getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
	*/

	public XnRegion asRegion() {
		return RealRegion.make(false, (PrimSpec.pointer().arrayWithTwo((BeforeReal.make(this)), (AfterReal.make(this)))));
		/*
		udanax-top.st:32027:RealPos methodsFor: 'accessing'!
		{XnRegion} asRegion
			^RealRegion make: false
				with: (PrimSpec pointer
					arrayWithTwo: (BeforeReal make: self)
					with: (AfterReal make: self))!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return RealSpace.make();
		/*
		udanax-top.st:32034:RealPos methodsFor: 'accessing'!
		{CoordinateSpace INLINE} coordinateSpace
			^RealSpace make!
		*/
	}

	/**
	 * Essential. Return the number as a PrimFloat object from which you can get it in a variety
	 * of representations.
	 */
	public abstract PrimFloatValue value();
		/*
		udanax-top.st:32038:RealPos methodsFor: 'accessing'!
		{PrimFloatValue CLIENT} value
			"Essential. Return the number as a PrimFloat object from which you can get it in a variety of representations."
			
			self subclassResponsibility!
		*/
	

	public int actualHashForEqual() {
		return (int) asIEEE64();
		//{
		//return asIEEE64().basicCast(UInt32);
		//}
		//translateOnly;
		///* >>> smalltalkOnly */
		//return asIEEE64().truncated();
		/* <<< smalltalkOnly */
		/*
		udanax-top.st:32045:RealPos methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			
			[^self asIEEE64 basicCast: UInt32] translateOnly.
			
			[^self asIEEE64 truncated] smalltalkOnly!
		*/
	}

	public boolean isEqual(Heaper other) {
		//MarkM.thingToDo(); /* 128 bit values */
		if (other instanceof RealPos) {
			RealPos r = (RealPos) other;
			return asIEEE64() == r.asIEEE64();
		} else {
			return false;
		}
		/*
		udanax-top.st:32051:RealPos methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper} 
			
			MarkM thingToDo. "128 bit values"
			other
				cast: RealPos into: [:r |
					^self asIEEE64 = r asIEEE64]
				others: [^false].
			^false "fodder"!
		*/
	}

	public boolean isGE(Position other) {
		return asIEEE64() >= ((RealPos) other).asIEEE64();
		/*
		udanax-top.st:32061:RealPos methodsFor: 'testing'!
		{BooleanVar} isGE: other {Position}
			^self asIEEE64 >= (other cast: RealPos) asIEEE64!
		*/
	}

//	public IntegerValue exponent() {
//		passe();
//		/*
//		udanax-top.st:32067:RealPos methodsFor: 'smalltalk: passe'!
//		{IntegerVar} exponent
//			self passe!
//		*/
//	}

//	/**
//	 * Whether the real number that this object represents is exactly representable in an
//	 * available IEEE precision.  Currently the answer is always TRUE, and the available
//	 * precisions are 8 (stupid precision), 32 (single precision), and 64 (double precision).  If
//	 * the answer is FALSE, the meaning of the messages 'precision' and 'asIEEE' remain to be
//	 * defined.
//	 */
//	public boolean isIEEE() {
//		passe();
//		return true;
//		/*
//		udanax-top.st:32071:RealPos methodsFor: 'smalltalk: passe'!
//		{BooleanVar} isIEEE
//			"Whether the real number that this object represents is exactly representable in an available IEEE precision.  Currently the answer is always TRUE, and the available precisions are 8 (stupid precision), 32 (single precision), and 64 (double precision).  If the answer is FALSE, the meaning of the messages 'precision' and 'asIEEE' remain to be defined."
//			self passe.
//			^true!
//		*/
//	}

//	/**
//	 * This number represents exactly this->mantissa() * 2 ^ this->exponent().  Should we
//	 * eventually support real numbers which cannot be expressed exactly with integral mantissa
//	 * and exponent, then this message (and 'exponent') will BLAST for such numbers.
//	 */
//	public IntegerValue mantissa() {
//		passe();
//		/*
//		udanax-top.st:32076:RealPos methodsFor: 'smalltalk: passe'!
//		{IntegerVar} mantissa
//			"This number represents exactly this->mantissa() * 2 ^ this->exponent().  Should we eventually support real numbers which cannot be expressed exactly with integral mantissa and exponent, then this message (and 'exponent') will BLAST for such numbers."
//			self passe!
//		*/
//	}

	/**
	 * Returns the value as IEEE basic data type is big enough to hold any value which can be put
	 * into an XuReal.  Currently this is an IEEE64 (double precision).  In future releases of
	 * this API, the return type of this method may be changed to IEEE128 (quad precision).  Once
	 * we support other ways of representing real numbers, there may not be an all-inclusive IEEE
	 * type, in which case this message will BLAST.
	 * The only IEEE values which this will return are those that represent real numbers.  I.e.,
	 * no NANs, no inifinities, no negative zero.
	 */
	public abstract double asIEEE();
		/*
		udanax-top.st:32083:RealPos methodsFor: 'obsolete:'!
		{IEEE64} asIEEE
			"Returns the value as IEEE basic data type is big enough to hold any value which can be put into an XuReal.  Currently this is an IEEE64 (double precision).  In future releases of this API, the return type of this method may be changed to IEEE128 (quad precision).  Once we support other ways of representing real numbers, there may not be an all-inclusive IEEE type, in which case this message will BLAST.
			
			The only IEEE values which this will return are those that represent real numbers.  I.e., no NANs, no inifinities, no negative zero."
			
			self subclassResponsibility!
		*/
	

	/**
	 * Returns the value as IEEE64 (double precision).
	 * The only IEEE values which this will return are those that represent real numbers.  I.e.,
	 * no NANs, no inifinities, no negative zero.
	 */
	public abstract double asIEEE64();
		/*
		udanax-top.st:32090:RealPos methodsFor: 'obsolete:'!
		{IEEE64} asIEEE64
			"Returns the value as IEEE64 (double precision).
			The only IEEE values which this will return are those that represent real numbers.  I.e., no NANs, no inifinities, no negative zero."
			
			self subclassResponsibility!
		*/
	

	/**
	 * What precision is it, in terms of the number of bits used to represent it.  In the
	 * interests of efficiency, this may return a number larger than that *needed* to represent
	 * it.  However, the precision reported must be at least that needed to represent this
	 * number.  It is assumed that the format of the number satisfies the IEEE radix independent
	 * floating point spec.  Should we represent real numbers other that those representable in
	 * IEEE, the meaning of this message will be more fully specified.
	 * The fact that this message is allowed to overestimate precision doesn't interfere with
	 * equality: a->isEqual(b) exactly when they represent that same real number, even if one of
	 * them happens to overestimate precision more that the other.
	 */
	public abstract int precision();
//		MarkM.thingToDo();
//		/* retire this */
//		Abstract.subclassResponsibility();
		/*
		udanax-top.st:32096:RealPos methodsFor: 'obsolete:'!
		{Int32} precision
			"What precision is it, in terms of the number of bits used to represent it.  In the interests of efficiency, this may return a number larger than that *needed* to represent it.  However, the precision reported must be at least that needed to represent this number.  It is assumed that the format of the number satisfies the IEEE radix independent floating point spec.  Should we represent real numbers other that those representable in IEEE, the meaning of this message will be more fully specified.
			
			The fact that this message is allowed to overestimate precision doesn't interfere with equality: a->isEqual(b) exactly when they represent that same real number, even if one of them happens to overestimate precision more that the other."
			
			MarkM thingToDo. "retire this"
			self subclassResponsibility!
		*/
	

	protected RealPos() {
		super();
	}

	public RealPos(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:32106:RealPos methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:32109:RealPos methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	/**
	 * make an XuReal given an IEEE floating point number of whatever precision on this platform
	 * is able to hold all the real numbers currently representable by an XuReal.  Currently this
	 * is IEEE64 (double precision), but may be redeclared as a larger IEEE precision in the
	 * future.  See comment in XuReal::makeIEEE64
	 */
	public static RealPos make(double value) {
		return makeIEEE64(value);
		/*
		udanax-top.st:32121:RealPos class methodsFor: 'creation'!
		{RealPos INLINE} make: value {IEEE64}
			"make an XuReal given an IEEE floating point number of whatever precision on this platform is able to hold all the real numbers currently representable by an XuReal.  Currently this is IEEE64 (double precision), but may be redeclared as a larger IEEE precision in the future.  See comment in XuReal::makeIEEE64"
			^self makeIEEE64: value!
		*/
	}

	/**
	 * See comment in XuReal::makeIEEE64
	 */
	public static RealPos makeIEEE32(float value) {
//		TODO knownBug(); /* must ensure that it is a number, and convert -0 to +0 */
//		TODO thingToDo(); * perhaps we should check to see if a lower precision can hold it exactly, and delegate to XuIEEE8.  Nahh. */
		return new IEEE32Pos(value);
		/*
		udanax-top.st:32126:RealPos class methodsFor: 'creation'!
		{RealPos} makeIEEE32: value {IEEE32}
			"See comment in XuReal::makeIEEE64"
			self knownBug. "must ensure that it is a number, and convert -0 to +0"
			self thingToDo. "perhaps we should check to see if a lower precision can hold it exactly, and delegate to XuIEEE8.  Nahh."
			
			^IEEE32Pos create: value!
		*/
	}

	/**
	 * Returns an XuReal which exactly represents the same real number that is represented by
	 * 'value'.  BLASTs if value doesn't represent a real (i.e., no NANs or inifinities).
	 * Negative 0 will be silently converted to positive zero
	 */
	public static RealPos makeIEEE64(double value) {
//		TODO knownBug(); /* must ensure that it is a number, and convert -0 to +0 */
//		TODO thingToDo(); /* perhaps we should check to see if a lower precision can hold it exactly, and delegate to XuIEEE32 or XuIEEE8.  Nahh. */
		return new IEEE64Pos(value);
		/*
		udanax-top.st:32134:RealPos class methodsFor: 'creation'!
		{RealPos} makeIEEE64: value {IEEE64}
			"Returns an XuReal which exactly represents the same real number that is represented by 'value'.  BLASTs if value doesn't represent a real (i.e., no NANs or inifinities).  Negative 0 will be silently converted to positive zero"
			self knownBug. "must ensure that it is a number, and convert -0 to +0"
			self thingToDo. "perhaps we should check to see if a lower precision can hold it exactly, and delegate to XuIEEE32 or XuIEEE8.  Nahh."
			
			^IEEE64Pos create: value!
		*/
	}

//	/**
//	 * See comment in XuReal::makeIEEE64
//	 */
//	public static RealPos makeIEEE8(IEEE8 value) {
//		knownBug();
//		/* must ensure that it is a number, and convert -0 to +0 */
//		return new IEEE8Pos(value);
//		/*
//		udanax-top.st:32142:RealPos class methodsFor: 'creation'!
//		{RealPos} makeIEEE8: value {IEEE8}
//			"See comment in XuReal::makeIEEE64"
//			self knownBug. "must ensure that it is a number, and convert -0 to +0"
//			
//			^IEEE8Pos create: value!
//		*/
//	}

	/**
	 * {PrimFloat CLIENT} value
	 */
	public static void info() {
		/*
		udanax-top.st:32151:RealPos class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{PrimFloat CLIENT} value
		"!
		*/
	}

	public static String exportName() {
		return "Real";
		/*
		udanax-top.st:32157:RealPos class methodsFor: 'smalltalk: promise'!
		exportName
			^'Real'!
		*/
	}
}
