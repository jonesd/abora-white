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

import info.dgjones.abora.white.cache.InstanceCache;
import info.dgjones.abora.white.collection.arrays.IntegerVarArray;
import info.dgjones.abora.white.collection.steppers.Accumulator;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class IntegerEdgeAccumulator extends Accumulator {
	protected boolean myStartsInside;
	protected IntegerVarArray myEdges;
	protected int myIndex;
	protected boolean havePending;
	protected IntegerValue myPending;
	protected static InstanceCache SomeAccumulators = InstanceCache.make(16);
	/*
	udanax-top.st:12132:
	Accumulator subclass: #IntegerEdgeAccumulator
		instanceVariableNames: '
			myStartsInside {BooleanVar}
			myEdges {IntegerVarArray}
			myIndex {UInt32}
			havePending {BooleanVar}
			myPending {IntegerVar}'
		classVariableNames: 'SomeAccumulators {InstanceCache} '
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Integers'!
	*/
	/*
	udanax-top.st:12141:
	(IntegerEdgeAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; yourself)!
	*/
	/*
	udanax-top.st:12237:
	IntegerEdgeAccumulator class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12240:
	(IntegerEdgeAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; yourself)!
	*/

	public IntegerEdgeAccumulator(boolean startsInside, int count) {
		super();
		myStartsInside = startsInside;
		myEdges = IntegerVarArray.make(count);
		myIndex = 0;
		havePending = false;
		myPending = IntegerValue.zero();
		/*
		udanax-top.st:12146:IntegerEdgeAccumulator methodsFor: 'protected: creation'!
		create: startsInside {BooleanVar} with: count {UInt32}
			super create.
			myStartsInside _ startsInside.
			myEdges _ IntegerVarArray zeros: count.
			myIndex _ Int32Zero.
			havePending _ false.
			myPending _ IntegerVar0!
		*/
	}

	public IntegerEdgeAccumulator(boolean startsInside, IntegerVarArray edges, int index, boolean hasPending, IntegerValue pending) {
		super();
		myStartsInside = startsInside;
		myEdges = edges;
		myIndex = index;
		havePending = hasPending;
		myPending = pending;
		/*
		udanax-top.st:12154:IntegerEdgeAccumulator methodsFor: 'protected: creation'!
		create: startsInside {BooleanVar} with: edges {IntegerVarArray} with: index {UInt32} with: hasPending {BooleanVar} with: pending {IntegerVar}
			super create.
			myStartsInside _ startsInside.
			myEdges _ edges.
			myIndex _ index.
			havePending _ hasPending.
			myPending _ pending!
		*/
	}

	public Accumulator copy() {
		Heaper result;
		result = SomeAccumulators.fetch();
		if (result == null) {
			return new IntegerEdgeAccumulator(myStartsInside, myEdges, myIndex, havePending, myPending);
		} else {
			//TODO review
			return new IntegerEdgeAccumulator(myStartsInside, myEdges, myIndex, havePending, myPending);
		}
		/*
		udanax-top.st:12164:IntegerEdgeAccumulator methodsFor: 'creation'!
		{Accumulator} copy
			| result {Heaper} |
			result := SomeAccumulators fetch.
			result == NULL
				ifTrue: [
					^IntegerEdgeAccumulator create: myStartsInside with: myEdges with: myIndex with: havePending with: myPending]
				ifFalse: [	
						^(IntegerEdgeAccumulator new.Become: result)
							create: myStartsInside with: myEdges with: myIndex with: havePending with: myPending]!
		*/
	}

	public void destroy() {
		if (!(SomeAccumulators.store(this))) {
			super.destroy();
		}
		/*
		udanax-top.st:12174:IntegerEdgeAccumulator methodsFor: 'creation'!
		{void} destroy
			(SomeAccumulators store: self) ifFalse: [super destroy]!
		*/
	}

	public void step(Heaper someObj) {
		edge(((IntegerPos) someObj).asIntegerVar());
		/*
		udanax-top.st:12179:IntegerEdgeAccumulator methodsFor: 'operations'!
		{void} step: someObj {Heaper}
			self edge: (someObj cast: IntegerPos) asIntegerVar!
		*/
	}

	public Heaper value() {
		return region();
		/*
		udanax-top.st:12182:IntegerEdgeAccumulator methodsFor: 'operations'!
		{Heaper} value
			^self region!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(region());
		oo.print(")");
		/*
		udanax-top.st:12187:IntegerEdgeAccumulator methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << self region << ')'!
		*/
	}

	/**
	 * add a transition at the given position. doing it again cancels it.  This particular coding
	 * is used for C++ inlinability
	 */
	public void edge(IntegerValue x) {
		if (havePending) {
			if (myPending == x) {
				havePending = false;
			} else {
				myEdges.storeIntegerVar(myIndex, myPending);
				myIndex = myIndex + 1;
				myPending = x;
			}
		} else {
			havePending = true;
			myPending = x;
		}
		/*
		udanax-top.st:12192:IntegerEdgeAccumulator methodsFor: 'edge operations'!
		{void} edge: x {IntegerVar}
			"add a transition at the given position. doing it again cancels it.  This particular coding is used for C++ inlinability"
			havePending ifTrue:
				[myPending = x
					ifTrue:
						[havePending _ false]
					ifFalse:
						[myEdges at: myIndex storeIntegerVar: myPending.
						myIndex _ myIndex + 1.
						myPending _ x]]
			ifFalse:
				[havePending _ true.
				myPending _ x].!
		*/
	}

	/**
	 * add a whole bunch of edges at once, assuming that they are sorted and there are no
	 * duplicates
	 */
	public void edges(IntegerEdgeStepper stepper) {
		if (stepper.hasValue()) {
			edge(stepper.edge());
			stepper.step();
			if (stepper.hasValue()) {
				if (!havePending) {
					myPending = stepper.edge();
					havePending = true;
					stepper.step();
				}
				while (stepper.hasValue()) {
					myEdges.storeIntegerVar(myIndex, myPending);
					myIndex = myIndex + 1;
					myPending = stepper.edge();
					stepper.step();
				}
			}
		}
		/*
		udanax-top.st:12206:IntegerEdgeAccumulator methodsFor: 'edge operations'!
		{void} edges: stepper {IntegerEdgeStepper}
			"add a whole bunch of edges at once, assuming that they are sorted and there are no duplicates"
			stepper hasValue ifTrue:
				[self edge: stepper edge.
				stepper step.
				stepper hasValue ifTrue:
					[havePending ifFalse:
						[myPending _ stepper edge.
						havePending _ true.
						stepper step].
					[stepper hasValue] whileTrue:
						[myEdges at: myIndex storeIntegerVar: myPending.
						myIndex _ myIndex + 1.
						myPending _ stepper edge.
						stepper step]]]!
		*/
	}

	/**
	 * make a region out of the accumulated edges
	 */
	public IntegerRegion region() {
		if (havePending) {
			myEdges.storeIntegerVar(myIndex, myPending);
			return new IntegerRegion(myStartsInside, myIndex + 1, myEdges);
		} else {
			if (myIndex == 0) {
				if (myStartsInside) {
					return IntegerRegion.allIntegers();
				} else {
					return IntegerRegion.make();
				}
			} else {
				return new IntegerRegion(myStartsInside, myIndex, myEdges);
			}
		}
		/*
		udanax-top.st:12222:IntegerEdgeAccumulator methodsFor: 'edge operations'!
		{IntegerRegion} region
			"make a region out of the accumulated edges"
			
			havePending ifTrue:
				[myEdges at: myIndex storeIntegerVar: myPending.
				^IntegerRegion create: myStartsInside with: myIndex + 1 with: myEdges]
			ifFalse:
				[myIndex == Int32Zero ifTrue:
					[myStartsInside
						ifTrue: [^IntegerRegion allIntegers]
						ifFalse: [^IntegerRegion make]]
				ifFalse: 
					[^IntegerRegion create: myStartsInside with: myIndex with: myEdges]]!
		*/
	}

	public static IntegerEdgeAccumulator make(boolean startsInside, int count) {
		Heaper result;
		result = SomeAccumulators.fetch();
		if (result == null) {
			return new IntegerEdgeAccumulator(startsInside, count);
		} else {
			//TODO review
			return new IntegerEdgeAccumulator(startsInside, count);
		}
		/*
		udanax-top.st:12245:IntegerEdgeAccumulator class methodsFor: 'creation'!
		make: startsInside {BooleanVar} with: count {UInt32}
			| result {Heaper} |
			result := SomeAccumulators fetch.
			result == NULL
				ifTrue: [^ self create: startsInside with: count]
				ifFalse: [^ (self new.Become: result) create: startsInside with: count]!
		*/
	}

//	public static void initTimeNonInherited() {
//		SomeAccumulators = InstanceCache.make(16);
//		/*
//		udanax-top.st:12254:IntegerEdgeAccumulator class methodsFor: 'smalltalk: init'!
//		initTimeNonInherited
//			SomeAccumulators := InstanceCache make: 16!
//		*/
//	}

//	public static void linkTimeNonInherited() {
//		SomeAccumulators = null;
//		/*
//		udanax-top.st:12257:IntegerEdgeAccumulator class methodsFor: 'smalltalk: init'!
//		linkTimeNonInherited
//			SomeAccumulators := NULL!
//		*/
//	}
}
