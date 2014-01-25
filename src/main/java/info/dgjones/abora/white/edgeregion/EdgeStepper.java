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
package info.dgjones.abora.white.edgeregion;

import info.dgjones.abora.white.cache.InstanceCache;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A single instance of this class is cached.  To take advantage of this, a method
 * that uses EdgeSteppers should explicitly destroy at least one of them.
 * Consider this a "protected" class.  See class comment in EdgeAccumulator.
 */
public class EdgeStepper extends Stepper {
	protected boolean myEntering;
	protected PtrArray myEdges;
	protected int myEdgesCount;
	protected int myIndex;
	protected static InstanceCache SomeEdgeSteppers = InstanceCache.make(16);
	/*
	udanax-top.st:53622:
	Stepper subclass: #EdgeStepper
		instanceVariableNames: '
			myEntering {BooleanVar}
			myEdges {PtrArray of: TransitionEdge}
			myEdgesCount {Int32}
			myIndex {Int32}'
		classVariableNames: 'SomeEdgeSteppers {InstanceCache} '
		poolDictionaries: ''
		category: 'Xanadu-EdgeRegion'!
	*/
	/*
	udanax-top.st:53630:
	EdgeStepper comment:
	'A single instance of this class is cached.  To take advantage of this, a method
	that uses EdgeSteppers should explicitly destroy at least one of them.
	Consider this a "protected" class.  See class comment in EdgeAccumulator.'!
	*/
	/*
	udanax-top.st:53634:
	(EdgeStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; yourself)!
	*/
	/*
	udanax-top.st:53708:
	EdgeStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:53711:
	(EdgeStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; yourself)!
	*/

	public Heaper fetch() {
		if (myIndex < myEdgesCount) {
			return myEdges.fetch(myIndex);
		} else {
			return null;
		}
		/*
		udanax-top.st:53639:EdgeStepper methodsFor: 'accessing'!
		{Heaper wimpy} fetch
			myIndex < myEdgesCount
				ifTrue: [^myEdges fetch: myIndex]
				ifFalse: [^NULL]!
		*/
	}

	public boolean hasValue() {
		return myIndex < myEdgesCount;
		/*
		udanax-top.st:53645:EdgeStepper methodsFor: 'accessing'!
		{BooleanVar} hasValue
			^myIndex < myEdgesCount!
		*/
	}

	public void step() {
		if (hasValue()) {
			myEntering = !myEntering;
			myIndex = myIndex + 1;
		}
		/*
		udanax-top.st:53649:EdgeStepper methodsFor: 'accessing'!
		{void} step
			self hasValue ifTrue:
				[myEntering := myEntering not.
				myIndex := myIndex + 1]!
		*/
	}

	public TransitionEdge fetchEdge() {
		if (myIndex < myEdgesCount) {
			return (TransitionEdge) (myEdges.fetch(myIndex));
		} else {
			return null;
		}
		/*
		udanax-top.st:53657:EdgeStepper methodsFor: 'edge accessing'!
		{TransitionEdge | NULL} fetchEdge
			myIndex < myEdgesCount
				ifTrue: [^(myEdges fetch: myIndex) cast: TransitionEdge]
				ifFalse: [^NULL]!
		*/
	}

	public TransitionEdge getEdge() {
		if (myIndex < myEdgesCount) {
			return (TransitionEdge) (myEdges.fetch(myIndex));
		} else {
			throw new AboraRuntimeException(AboraRuntimeException.EMPTY_STEPPER);
		}
		/*
		udanax-top.st:53663:EdgeStepper methodsFor: 'edge accessing'!
		{TransitionEdge} getEdge
			myIndex < myEdgesCount
				ifTrue: [^(myEdges fetch: myIndex) cast: TransitionEdge]
				ifFalse: [Heaper BLAST: #EmptyStepper].
			^NULL "fodder"!
		*/
	}

	/**
	 * whether the current transition is entering or leaving the set
	 */
	public boolean isEntering() {
		return myEntering;
		/*
		udanax-top.st:53670:EdgeStepper methodsFor: 'edge accessing'!
		{BooleanVar} isEntering
			"whether the current transition is entering or leaving the set"
			^myEntering!
		*/
	}

	public EdgeStepper(boolean entering, PtrArray edges, int count) {
		super();
		myEntering = entering;
		myEdges = edges;
		myEdgesCount = count;
		myIndex = 0;
		/*
		udanax-top.st:53676:EdgeStepper methodsFor: 'protected: create'!
		create: entering {BooleanVar} with: edges {PtrArray of: TransitionEdge} with: count {Int32}
			super create.
			myEntering := entering.
			myEdges := edges.
			myEdgesCount := count.
			myIndex := Int32Zero!
		*/
	}

	public EdgeStepper(boolean entering, PtrArray edges, int count, int index) {
		super();
		myEntering = entering;
		myEdges = edges;
		myEdgesCount = count;
		myIndex = index;
		/*
		udanax-top.st:53684:EdgeStepper methodsFor: 'protected: create'!
		create: entering {BooleanVar} with: edges {PtrArray of: TransitionEdge} with: count {Int32} with: index {Int32}
			super create.
			myEntering := entering.
			myEdges := edges.
			myEdgesCount := count.
			myIndex := index!
		*/
	}

	public Stepper copy() {
		return new EdgeStepper(myEntering, myEdges, myEdgesCount, myIndex);
		/*
		udanax-top.st:53694:EdgeStepper methodsFor: 'create'!
		{Stepper} copy
			^EdgeStepper create: myEntering
				with: myEdges
				with: myEdgesCount
				with: myIndex!
		*/
	}

	public void destroy() {
		if (!(SomeEdgeSteppers.store(this))) {
			super.destroy();
		}
		/*
		udanax-top.st:53703:EdgeStepper methodsFor: 'destroy'!
		{void} destroy
			(SomeEdgeSteppers store: self) ifFalse:
				[super destroy]!
		*/
	}

	public static EdgeStepper make(boolean entering, PtrArray edges) {
		Heaper result = SomeEdgeSteppers.fetch();
		if (result == null) {
			return new EdgeStepper(entering, edges, edges.count());
		} else {
			// TODO review new
			return new EdgeStepper(entering, edges, edges.count());
		}
		/*
		udanax-top.st:53716:EdgeStepper class methodsFor: 'create'!
		make: entering {BooleanVar} with: edges {PtrArray of: TransitionEdge}
			| result {Heaper} |
			result := SomeEdgeSteppers fetch.
			result == NULL ifTrue: [
				^ self create: entering with: edges with: edges count]
			ifFalse: [
				^ (self new.Become: result) create: entering with: edges with: edges count]!
		*/
	}

	public static EdgeStepper make(boolean entering, PtrArray edges, int count) {
		Heaper result = SomeEdgeSteppers.fetch();
		if (result == null) {
			return new EdgeStepper(entering, edges, count);
		} else {
			//TODO review new
			return new EdgeStepper(entering, edges, count);
		}
		/*
		udanax-top.st:53724:EdgeStepper class methodsFor: 'create'!
		make: entering {BooleanVar} with: edges {PtrArray of: TransitionEdge} with: count {Int32}
			| result {Heaper} |
			result := SomeEdgeSteppers fetch.
			result == NULL ifTrue: [
				^ self create: entering with: edges with: count]
			ifFalse: [
				^ (self new.Become: result) create: entering with: edges with: count]!
		*/
	}

	//	public static void initTimeNonInherited() {
	//		SomeEdgeSteppers = InstanceCache.make(16);
	//		/*
	//		udanax-top.st:53734:EdgeStepper class methodsFor: 'smalltalk: init'!
	//		initTimeNonInherited
	//			SomeEdgeSteppers := InstanceCache make: 16!
	//		*/
	//	}

	//	public static void linkTimeNonInherited() {
	//		SomeEdgeSteppers = null;
	//		/*
	//		udanax-top.st:53737:EdgeStepper class methodsFor: 'smalltalk: init'!
	//		linkTimeNonInherited
	//			SomeEdgeSteppers := NULL!
	//		*/
	//	}
}
