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
package info.dgjones.abora.white.filter;

import java.io.PrintWriter;

import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class ClosedFilter extends Filter {
	/*
	udanax-top.st:66784:
	Filter subclass: #ClosedFilter
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Filter'!
	*/
	/*
	udanax-top.st:66788:
	(ClosedFilter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:66898:
	ClosedFilter class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:66901:
	(ClosedFilter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	protected ClosedFilter(FilterSpace cs) {
		super(cs);
		/*
		udanax-top.st:66793:ClosedFilter methodsFor: 'creation'!
		create: cs {FilterSpace}
			super create: cs!
		*/
	}

	public XnRegion complement() {
		return Filter.openFilter(coordinateSpace());
		/*
		udanax-top.st:66798:ClosedFilter methodsFor: 'operations'!
		{XnRegion} complement
			^Filter openFilter: self coordinateSpace!
		*/
	}

	public XnRegion intersect(XnRegion other) {
		return this;
		/*
		udanax-top.st:66802:ClosedFilter methodsFor: 'operations'!
		{XnRegion} intersect: other {XnRegion unused}
			^self!
		*/
	}

	public XnRegion minus(XnRegion other) {
		return this;
		/*
		udanax-top.st:66805:ClosedFilter methodsFor: 'operations'!
		{XnRegion} minus: other {XnRegion unused}
			^self!
		*/
	}

	public XnRegion unionWith(XnRegion other) {
		return other;
		/*
		udanax-top.st:66808:ClosedFilter methodsFor: 'operations'!
		{XnRegion} unionWith: other {XnRegion}
			^other!
		*/
	}

	/**
	 * tell whether a region passes this filter
	 */
	public boolean match(XnRegion region) {
		return false;
		/*
		udanax-top.st:66813:ClosedFilter methodsFor: 'filtering'!
		{BooleanVar} match: region {XnRegion unused}
			"tell whether a region passes this filter"
			^false!
		*/
	}

	/**
	 * return the simplest filter for looking at the children
	 */
	public Filter pass(Joint parent) {
		return this;
		/*
		udanax-top.st:66817:ClosedFilter methodsFor: 'filtering'!
		{Filter} pass: parent {Joint unused}
			"return the simplest filter for looking at the children"
			^self!
		*/
	}

	public int actualHashForEqual() {
		return coordinateSpace().hashForEqual() ^ getClass().hashCode();
		/*
		udanax-top.st:66823:ClosedFilter methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^self coordinateSpace hashForEqual bitXor: #cat.U.ClosedFilter hashForEqual!
		*/
	}

	public boolean isAllFilter() {
		return false;
		/*
		udanax-top.st:66826:ClosedFilter methodsFor: 'testing'!
		{BooleanVar} isAllFilter
			^false!
		*/
	}

	public boolean isAnyFilter() {
		return true;
		/*
		udanax-top.st:66830:ClosedFilter methodsFor: 'testing'!
		{BooleanVar} isAnyFilter
			
			^true!
		*/
	}

	public boolean isEmpty() {
		return true;
		/*
		udanax-top.st:66834:ClosedFilter methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^true!
		*/
	}

	public boolean isEnumerable(OrderSpec order) {
		return true;
		/*
		udanax-top.st:66837:ClosedFilter methodsFor: 'testing'!
		{BooleanVar} isEnumerable: order {OrderSpec default: NULL}
			
			^true!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof ClosedFilter) {
			ClosedFilter cf = (ClosedFilter) other;
			return cf.coordinateSpace().isEqual(coordinateSpace());
		} else {
			return false;
		}
		/*
		udanax-top.st:66841:ClosedFilter methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: ClosedFilter into: [:cf |
					^cf coordinateSpace isEqual: self coordinateSpace]
				others: [^false].
			^false "fodder"!
		*/
	}

	public boolean isFull() {
		return false;
		/*
		udanax-top.st:66849:ClosedFilter methodsFor: 'testing'!
		{BooleanVar} isFull	
			^false!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(coordinateSpace());
		oo.print(")");
		/*
		udanax-top.st:66855:ClosedFilter methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << self coordinateSpace << ')'!
		*/
	}

	public XnRegion fetchSpecialSubset(XnRegion other) {
		return this;
		/*
		udanax-top.st:66860:ClosedFilter methodsFor: 'protected: protected operations'!
		{XnRegion} fetchSpecialSubset: other {XnRegion unused}
			^self!
		*/
	}

	public Stepper actualStepper(OrderSpec order) {
		return Stepper.emptyStepper();
		/*
		udanax-top.st:66865:ClosedFilter methodsFor: 'protected: enumerating'!
		{Stepper of: Position} actualStepper: order {OrderSpec} 
			
			^Stepper emptyStepper!
		*/
	}

	public Stepper intersectedFilters() {
		return Stepper.itemStepper(this);
		/*
		udanax-top.st:66871:ClosedFilter methodsFor: 'enumerating'!
		{Stepper of: Filter} intersectedFilters
			^Stepper itemStepper: self!
		*/
	}

	public Stepper unionedFilters() {
		return Stepper.emptyStepper();
		/*
		udanax-top.st:66875:ClosedFilter methodsFor: 'enumerating'!
		{Stepper of: Filter} unionedFilters
			^Stepper emptyStepper!
		*/
	}

	public XnRegion baseRegion() {
		return ((FilterSpace) coordinateSpace()).emptyRegion();
		/*
		udanax-top.st:66881:ClosedFilter methodsFor: 'accessing'!
		{XnRegion} baseRegion
			^(self coordinateSpace cast: FilterSpace) emptyRegion!
		*/
	}

	public XnRegion relevantRegion() {
		return filterSpace().baseSpace().emptyRegion();
		/*
		udanax-top.st:66885:ClosedFilter methodsFor: 'accessing'!
		{XnRegion} relevantRegion
			^self filterSpace baseSpace emptyRegion!
		*/
	}

	protected ClosedFilter(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:66891:ClosedFilter methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:66894:ClosedFilter methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static Filter make(CoordinateSpace space) {
		return new ClosedFilter(((FilterSpace) space));
		/*
		udanax-top.st:66906:ClosedFilter class methodsFor: 'pseudo constructors'!
		{Filter} make: space {CoordinateSpace}
			^self create: (space cast: FilterSpace)!
		*/
	}
}
