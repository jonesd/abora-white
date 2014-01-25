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

import info.dgjones.abora.white.arrange.Arrangement;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class SequenceUpOrder extends OrderSpec {
	/*
	udanax-top.st:31117:
	OrderSpec subclass: #SequenceUpOrder
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:31121:
	(SequenceUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:31177:
	SequenceUpOrder class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:31180:
	(SequenceUpOrder getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected SequenceUpOrder() {
		super();
	}

	protected SequenceUpOrder(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:31170:SequenceUpOrder methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods
	
	public static OrderSpec make() {
		return new SequenceUpOrder();
		/*
		udanax-top.st:31185:SequenceUpOrder class methodsFor: 'pseudo constructors'!
		{OrderSpec} make
			^self create!
		*/
	}

	/////////////////////////////////////////////
	// Testing

	public int actualHashForEqual() {
		return getClass().hashCode();
//		return getCategory().hashForEqual();
		/*
		udanax-top.st:31126:SequenceUpOrder methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^self getCategory hashForEqual!
		*/
	}

	public boolean follows(Position x, Position y) {
		return (((Sequence) x).secretNumbers().compare(((Sequence) y).secretNumbers())) >= 0;
		/*
		udanax-top.st:31130:SequenceUpOrder methodsFor: 'testing'!
		{BooleanVar} follows: x {Position} with: y {Position}
			^((x cast: Sequence) secretNumbers
				compare: (y cast: Sequence) secretNumbers) >= Int32Zero!
		*/
	}

	public boolean isEqual(Heaper other) {
		return other instanceof SequenceUpOrder;
		/*
		udanax-top.st:31135:SequenceUpOrder methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			^other isKindOf: SequenceUpOrder!
		*/
	}

	public boolean isFullOrder(XnRegion keys) {
		return true;
		/*
		udanax-top.st:31139:SequenceUpOrder methodsFor: 'testing'!
		{BooleanVar} isFullOrder: keys {XnRegion unused default: NULL}
			^true!
		*/
	}

	public boolean preceeds(XnRegion before, XnRegion after) {
		SequenceRegion first;
		SequenceRegion second;
		first = (SequenceRegion) before;
		second = (SequenceRegion) after;
		if (!first.isBoundedBelow()) {
			return true;
		}
		if (!second.isBoundedBelow()) {
			return false;
		}
		return !(((SequenceEdge) (first.secretTransitions().fetch(0))).isGE(((SequenceEdge) (second.secretTransitions().fetch(0)))));
		/*
		udanax-top.st:31143:SequenceUpOrder methodsFor: 'testing'!
		{BooleanVar} preceeds: before {XnRegion} with: after {XnRegion}
			
			| first {SequenceRegion} second {SequenceRegion} |
			first _ before cast: SequenceRegion.
			second _ after cast: SequenceRegion.
			first isBoundedBelow ifFalse: [^true].
			second isBoundedBelow ifFalse: [^false].
			^(((first secretTransitions fetch: Int32Zero) cast: SequenceEdge)
				isGE: ((second secretTransitions fetch: Int32Zero) cast: SequenceEdge)) not!
		*/
	}

	/////////////////////////////////////////////
	// Accessing

	public Arrangement arrange(XnRegion region) {
		Stepper stepper;
		PtrArray array;
		if (!region.isFinite()) {
			throw new AboraRuntimeException(AboraRuntimeException.MUST_BE_FINITE);
		}
		stepper = ((SequenceRegion) region).stepper();
		array = (PtrArray) stepper.stepMany();
		if (!stepper.atEnd()) {
			throw new UnsupportedOperationException();
//			unimplemented();
		}
		return ExplicitArrangement.make(array);
		/*
		udanax-top.st:31155:SequenceUpOrder methodsFor: 'accessing'!
		{Arrangement} arrange: region {XnRegion}
			| stepper {Stepper} array {PtrArray} |
			region isFinite ifFalse: [Heaper BLAST: #MustBeFinite].
			stepper := (region cast: SequenceRegion) stepper.
			array := stepper stepMany cast: PtrArray.
			stepper atEnd ifFalse: [self unimplemented].
			^ExplicitArrangement make: array!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return SequenceSpace.make();
		/*
		udanax-top.st:31164:SequenceUpOrder methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^SequenceSpace make!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:31173:SequenceUpOrder methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
