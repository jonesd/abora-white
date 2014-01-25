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
package info.dgjones.abora.white.spaces.basic;

import info.dgjones.abora.white.arrange.Arrangement;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.integers.IntegerArrangement;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.spaces.integers.IntegerSpace;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class IntegerUpOrder extends OrderSpec {
	//TODO why isn't this a singleton?
	//TODO should we move class to org.abora.white.spaces.integers package
	
	/*
	udanax-top.st:30887:
	OrderSpec subclass: #IntegerUpOrder
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:30891:
	(IntegerUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:30955:
	IntegerUpOrder class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:30958:
	(IntegerUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected IntegerUpOrder() {
		super();
	}

	protected IntegerUpOrder(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:30948:IntegerUpOrder methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static OrderSpec make() {
		return new IntegerUpOrder();
		/*
		udanax-top.st:30963:IntegerUpOrder class methodsFor: 'pseudoconstructors'!
		{OrderSpec} make
			^self create!
		*/
	}
	
	/////////////////////////////////////////////
	// Testing

	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
		//		TODO return CAT.hashForEqual() + 1;
		/*
		udanax-top.st:30896:IntegerUpOrder methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^#cat.U.IntegerUpOrder hashForEqual + 1!
		*/
	}

	public boolean follows(Position x, Position y) {
		return ((IntegerPos) x).asIntegerVar().isGE(((IntegerPos) y).asIntegerVar());
		/*
		udanax-top.st:30900:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} follows: x {Position} with: y {Position}
			^(x cast: IntegerPos) asIntegerVar >= (y cast: IntegerPos) asIntegerVar!
		*/
	}

	/**
	 * See discussion in XuInteger class comment about boxed vs unboxed integers
	 */
	public boolean followsInt(IntegerValue x, IntegerValue y) {
		return x.isGE(y);
		/*
		udanax-top.st:30904:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} followsInt: x {IntegerVar} with: y {IntegerVar}
			"See discussion in XuInteger class comment about boxed vs unboxed integers"
			^ x >= y!
		*/
	}

	public boolean isEqual(Heaper other) {
		return other instanceof IntegerUpOrder;
		/*
		udanax-top.st:30909:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			^other isKindOf: IntegerUpOrder!
		*/
	}

	public boolean isFullOrder(XnRegion keys) {
		return true;
		/*
		udanax-top.st:30913:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} isFullOrder: keys {XnRegion unused default: NULL}
			
			^true!
		*/
	}

	/**
	 * Return true if some position in before is less than or equal to all positions in after.
	 */
	public boolean preceeds(XnRegion before, XnRegion after) {
		IntegerRegion first = (IntegerRegion) before;
		IntegerRegion second = (IntegerRegion) after;
		if (!first.isBoundedBelow()) {
			return true;
		}
		if (!second.isBoundedBelow()) {
			return false;
		}
		return first.start().isLE(second.start());
		/*
		udanax-top.st:30917:IntegerUpOrder methodsFor: 'testing'!
		{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
			"Return true if some position in before is less than or equal to all positions in after."
			
			| first {IntegerRegion} second {IntegerRegion} |
			first _ before cast: IntegerRegion.
			second _ after cast: IntegerRegion.
			first isBoundedBelow ifFalse: [^true].
			second isBoundedBelow ifFalse: [^false].
			^first start <= second start!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public Arrangement arrange(XnRegion region) {
		return IntegerArrangement.make(region, this);
		/*
		udanax-top.st:30929:IntegerUpOrder methodsFor: 'accessing'!
		{Arrangement} arrange: region {XnRegion}
			^IntegerArrangement make: region with: self.!
		*/
	}

	/**
	 * Return the first n positions in the region according to my ordering.
	 */
	public XnRegion chooseMany(XnRegion region, IntegerValue n) {
		return arrange(region).keysOf(0, n.asInt32());
		/*
		udanax-top.st:30932:IntegerUpOrder methodsFor: 'accessing'!
		{XnRegion} chooseMany: region {XnRegion} with: n {IntegerVar}
			"Return the first n positions in the region according to my ordering."
			
			^(self arrange: region) keysOf: Int32Zero with: n DOTasLong!
		*/
	}

	/**
	 * Return the first position in the region according to my ordering.
	 */
	public Position chooseOne(XnRegion region) {
		IntegerRegion integerRegion = (IntegerRegion) region; 
		return IntegerPos.make(integerRegion.start());
		/*
		udanax-top.st:30937:IntegerUpOrder methodsFor: 'accessing'!
		{Position} chooseOne: region {XnRegion}
			"Return the first position in the region according to my ordering."
			
			^IntegerPos make: (region cast: IntegerRegion) start!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:30942:IntegerUpOrder methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^IntegerSpace make!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:30951:IntegerUpOrder methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
