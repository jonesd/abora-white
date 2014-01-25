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
import info.dgjones.abora.white.collection.tables.Pair;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class NotSupersetFilter extends Filter {
	protected XnRegion myRegion;
	/*
	udanax-top.st:67046:
	Filter subclass: #NotSupersetFilter
		instanceVariableNames: 'myRegion {XnRegion}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Filter'!
	*/
	/*
	udanax-top.st:67050:
	(NotSupersetFilter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	/**
	 * tell whether a region passes this filter
	 */
	public boolean match(XnRegion region) {
		return !myRegion.isSubsetOf(region);
		/*
		udanax-top.st:67055:NotSupersetFilter methodsFor: 'filtering'!
		{BooleanVar} match: region {XnRegion}
			"tell whether a region passes this filter"
			^(myRegion isSubsetOf: region) not!
		*/
	}

	/**
	 * return the simplest filter for looking at the children
	 */
	public Filter pass(Joint parent) {
		if (!(myRegion.isSubsetOf(parent.unioned()))) {
			return Filter.openFilter(coordinateSpace());
		}
		if (myRegion.isSubsetOf(parent.intersected())) {
			return Filter.closedFilter(coordinateSpace());
		}
		return this;
		/*
		udanax-top.st:67059:NotSupersetFilter methodsFor: 'filtering'!
		{Filter} pass: parent {Joint}
			"return the simplest filter for looking at the children"
			(myRegion isSubsetOf: parent unioned) ifFalse:
				[^Filter openFilter: self coordinateSpace].
			(myRegion isSubsetOf: parent intersected) ifTrue:
				[^Filter closedFilter: self coordinateSpace].
			^self!
		*/
	}

	public XnRegion region() {
		return myRegion;
		/*
		udanax-top.st:67067:NotSupersetFilter methodsFor: 'filtering'!
		{XnRegion} region
			^myRegion!
		*/
	}

	public XnRegion complement() {
		return Filter.supersetFilter(coordinateSpace(), myRegion);
		/*
		udanax-top.st:67072:NotSupersetFilter methodsFor: 'operations'!
		{XnRegion} complement
			^Filter supersetFilter: self coordinateSpace with: myRegion!
		*/
	}

	public int actualHashForEqual() {
		return (coordinateSpace().hashForEqual() ^ myRegion.hashForEqual()) ^ getClass().hashCode();
		/*
		udanax-top.st:67078:NotSupersetFilter methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^(self coordinateSpace hashForEqual bitXor: myRegion hashForEqual)
				bitXor: #cat.U.NotSupersetFilter hashForEqual!
		*/
	}

	public boolean isAllFilter() {
		return false;
		/*
		udanax-top.st:67082:NotSupersetFilter methodsFor: 'testing'!
		{BooleanVar} isAllFilter
			^false!
		*/
	}

	public boolean isAnyFilter() {
		return false;
		/*
		udanax-top.st:67086:NotSupersetFilter methodsFor: 'testing'!
		{BooleanVar} isAnyFilter
			
			^false!
		*/
	}

	public boolean isEmpty() {
		return false;
		/*
		udanax-top.st:67090:NotSupersetFilter methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^false!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof NotSupersetFilter) {
			NotSupersetFilter nsf = (NotSupersetFilter) other;
			return nsf.region().isEqual(myRegion);
		} else {
			return false;
		}
		/*
		udanax-top.st:67093:NotSupersetFilter methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: NotSupersetFilter into: [:nsf |
					^nsf region isEqual: myRegion]
				others: [^false].
			^false "fodder"!
		*/
	}

	public boolean isFull() {
		return false;
		/*
		udanax-top.st:67101:NotSupersetFilter methodsFor: 'testing'!
		{BooleanVar} isFull	
			^false!
		*/
	}

	public NotSupersetFilter(FilterSpace cs, XnRegion region) {
		super(cs);
		myRegion = region;
		/*
		udanax-top.st:67107:NotSupersetFilter methodsFor: 'creation'!
		create: cs {FilterSpace} with: region {XnRegion}
			super create: cs.
			myRegion _ region!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(myRegion);
		oo.print(")");
		/*
		udanax-top.st:67113:NotSupersetFilter methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << myRegion << ')'!
		*/
	}

	/**
	 * return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)
	 */
	public Pair fetchCanonicalIntersect(Filter other) {
		if (other instanceof SubsetFilter) {
			SubsetFilter subF = (SubsetFilter) other;
			XnRegion others = subF.region();
			if (myRegion.isSubsetOf(others)) {
				return null;
			} else {
				return Pair.make((Filter.notSupersetFilter(coordinateSpace(), (myRegion.intersect(others)))), other);
			}
		} else if (other instanceof SupersetFilter) {
			SupersetFilter superF = (SupersetFilter) other;
			XnRegion others = superF.region();
			if (myRegion.intersects(others)) {
				return Pair.make((Filter.notSupersetFilter(coordinateSpace(), (myRegion.minus(superF.region())))), other);
			} else {
				return null;
			}
		} else {
			return null;
		}
		/*
		udanax-top.st:67118:NotSupersetFilter methodsFor: 'protected operations'!
		{Pair of: Filter} fetchCanonicalIntersect: other {Filter}
			"return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)"
			
			other
				cast: SubsetFilter into: [:subF |
					| others {XnRegion} |
					others _ subF region.
					(myRegion isSubsetOf: others)
						ifTrue: [^NULL]
						ifFalse: [^Pair make: (Filter notSupersetFilter: self coordinateSpace
								with: (myRegion intersect: others))
							with: other]]
				cast: SupersetFilter into: [:superF |
					| others {XnRegion} |
					others _ superF region.
					(myRegion intersects: others)
						ifTrue: [^Pair make: (Filter notSupersetFilter: self coordinateSpace
								with: (myRegion minus: superF region))
							with: other]
						ifFalse: [^NULL]]
				others: [^NULL].
			^NULL "fodder"!
		*/
	}

	/**
	 * return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)
	 */
	public Pair fetchCanonicalUnion(Filter other) {
		if (other instanceof SupersetFilter) {
			SupersetFilter sf = (SupersetFilter) other;
			XnRegion others = sf.region();
			if (myRegion.intersects(others)) {
				return Pair.make(this, (Filter.supersetFilter(coordinateSpace(), (sf.region().minus(myRegion)))));
			} else {
				return null;
			}
		} else {
			return null;
		}
		/*
		udanax-top.st:67141:NotSupersetFilter methodsFor: 'protected operations'!
		{Pair of: Filter} fetchCanonicalUnion: other {Filter}
			"return NULL, or the pair of canonical filters (left == new1 | self, right == new2 | other)"
			
			other
				cast: SupersetFilter into: [:sf |
					| others {XnRegion} |
					others _ sf region.
					(myRegion intersects: others)
						ifTrue: [^Pair make: self
							with: (Filter supersetFilter: self coordinateSpace
								with: (sf region minus: myRegion))]
						ifFalse: [^NULL]]
				others: [^NULL].
			^NULL "fodder"!
		*/
	}

	public XnRegion fetchSpecialIntersect(XnRegion other) {
		if (other instanceof SupersetFilter) {
			SupersetFilter sf = (SupersetFilter) other;
			if (myRegion.isSubsetOf(sf.region())) {
				return Filter.closedFilter(coordinateSpace());
			} else {
				return null;
			}
		} else {
			return null;
		}
		/*
		udanax-top.st:67156:NotSupersetFilter methodsFor: 'protected operations'!
		{XnRegion} fetchSpecialIntersect: other {XnRegion}
			other
				cast: SupersetFilter into: [:sf |
					(myRegion isSubsetOf: sf region)
						ifTrue: [^Filter closedFilter: self coordinateSpace]
						ifFalse: [^NULL]]
				others: [^NULL].
			^NULL "fodder"!
		*/
	}

	public XnRegion fetchSpecialSubset(XnRegion other) {
		if (other instanceof SubsetFilter) {
			SubsetFilter subF = (SubsetFilter) other;
			if (!(myRegion.isSubsetOf(subF.region()))) {
				return other;
			}
		} else if (other instanceof NotSupersetFilter) {
			NotSupersetFilter nSuperF = (NotSupersetFilter) other;
			XnRegion others = nSuperF.region();
			if (myRegion.isSubsetOf(others)) {
				return this;
			}
			if (others.isSubsetOf(myRegion)) {
				return other;
			}
		} else {
		}
		return null;
		/*
		udanax-top.st:67166:NotSupersetFilter methodsFor: 'protected operations'!
		{XnRegion} fetchSpecialSubset: other {XnRegion}
			other
				cast: SubsetFilter into: [:subF |
					(myRegion isSubsetOf: subF region)
						ifFalse: [^other]]
				cast: NotSupersetFilter into: [:nSuperF |
					| others {XnRegion} |
					others _ nSuperF region.
					(myRegion isSubsetOf: others) ifTrue: [^self].
					(others isSubsetOf: myRegion) ifTrue: [^other]]
				others: [].
			^NULL!
		*/
	}

	public XnRegion fetchSpecialUnion(XnRegion other) {
		if (other instanceof NotSubsetFilter) {
			NotSubsetFilter nSubF = (NotSubsetFilter) other;
			if (!(myRegion.isSubsetOf(nSubF.region()))) {
				return Filter.openFilter(coordinateSpace());
			}
		} else if (other instanceof SupersetFilter) {
			SupersetFilter superF = (SupersetFilter) other;
			if (superF.region().isSubsetOf(myRegion)) {
				return Filter.openFilter(coordinateSpace());
			}
		} else if (other instanceof NotSupersetFilter) {
			NotSupersetFilter nSuperF = (NotSupersetFilter) other;
			return Filter.notSupersetFilter(coordinateSpace(), (myRegion.unionWith(nSuperF.region())));
		} else {
		}
		return null;
		/*
		udanax-top.st:67180:NotSupersetFilter methodsFor: 'protected operations'!
		{XnRegion} fetchSpecialUnion: other {XnRegion}
			other
				cast: NotSubsetFilter into: [:nSubF |
					(myRegion isSubsetOf: nSubF region)
						ifFalse: [^Filter openFilter: self coordinateSpace]]
				cast: SupersetFilter into: [:superF |
					(superF region isSubsetOf: myRegion)
						ifTrue: [^Filter openFilter: self coordinateSpace]]
				cast: NotSupersetFilter into: [:nSuperF |
					^Filter notSupersetFilter: self coordinateSpace
						with: (myRegion unionWith: nSuperF region)]
				others: [].
			^NULL!
		*/
	}

	public Stepper intersectedFilters() {
		return Stepper.itemStepper(this);
		/*
		udanax-top.st:67197:NotSupersetFilter methodsFor: 'enumerating'!
		{Stepper of: Filter} intersectedFilters
			^Stepper itemStepper: self!
		*/
	}

	public Stepper unionedFilters() {
		return Stepper.itemStepper(this);
		/*
		udanax-top.st:67201:NotSupersetFilter methodsFor: 'enumerating'!
		{Stepper of: Filter} unionedFilters
			^Stepper itemStepper: self!
		*/
	}

	public XnRegion baseRegion() {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE_ENOUGH);
		/*
		udanax-top.st:67207:NotSupersetFilter methodsFor: 'accessing'!
		{XnRegion} baseRegion
			Heaper BLAST: #NotSimpleEnough.
			^NULL!
		*/
	}

	public XnRegion relevantRegion() {
		return myRegion;
		/*
		udanax-top.st:67212:NotSupersetFilter methodsFor: 'accessing'!
		{XnRegion} relevantRegion
			^myRegion!
		*/
	}

	public NotSupersetFilter(Rcvr receiver) {
		super(receiver);
		myRegion = (XnRegion) receiver.receiveHeaper();
		/*
		udanax-top.st:67218:NotSupersetFilter methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myRegion _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myRegion);
		/*
		udanax-top.st:67222:NotSupersetFilter methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myRegion.!
		*/
	}
}
