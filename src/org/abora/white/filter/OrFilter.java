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
package org.abora.white.filter;

import java.io.PrintWriter;

import org.abora.white.collection.sets.ImmuSet;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.xpp.basic.Heaper;

public class OrFilter extends Filter {
	protected ImmuSet mySubFilters;
	/*
	udanax-top.st:67342:
	Filter subclass: #OrFilter
		instanceVariableNames: 'mySubFilters {ImmuSet of: Filter}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Filter'!
	*/
	/*
	udanax-top.st:67346:
	(OrFilter getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	public OrFilter(FilterSpace cs, ImmuSet subs) {
		super(cs);
		mySubFilters = subs;
		/*
		udanax-top.st:67351:OrFilter methodsFor: 'creation'!
		create: cs {FilterSpace} with: subs {ImmuSet of: Filter}
			super create: cs.
			mySubFilters _ subs!
		*/
	}

	public int actualHashForEqual() {
		return (coordinateSpace().hashForEqual() ^ mySubFilters.hashForEqual()) ^ getClass().hashCode();
		/*
		udanax-top.st:67357:OrFilter methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^(self coordinateSpace hashForEqual bitXor: mySubFilters hashForEqual)
				bitXor: #cat.U.OrFilter hashForEqual!
		*/
	}

	public boolean isAllFilter() {
		return false;
		/*
		udanax-top.st:67361:OrFilter methodsFor: 'testing'!
		{BooleanVar} isAllFilter
			^false!
		*/
	}

	public boolean isAnyFilter() {
		return false;
		/*
		udanax-top.st:67365:OrFilter methodsFor: 'testing'!
		{BooleanVar} isAnyFilter
			
			^false!
		*/
	}

	public boolean isEmpty() {
		return false;
		/*
		udanax-top.st:67369:OrFilter methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^false!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof OrFilter) {
			OrFilter of = (OrFilter) other;
			return of.subFilters().isEqual(subFilters());
		} else {
			return false;
		}
		/*
		udanax-top.st:67372:OrFilter methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: OrFilter into: [:of |
					^of subFilters isEqual: self subFilters]
				others: [^false].
			^false "fodder"!
		*/
	}

	public boolean isFull() {
		return false;
		/*
		udanax-top.st:67380:OrFilter methodsFor: 'testing'!
		{BooleanVar} isFull	
			^false!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		subFilters().printOnWithSimpleSyntax(oo, "(", " || ", ")");
		/*
		udanax-top.st:67386:OrFilter methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name.
			self subFilters printOnWithSimpleSyntax: oo with: '(' with: ' || ' with: ')'!
		*/
	}

	public XnRegion complement() {
		XnRegion result = Filter.openFilter(coordinateSpace());
		Stepper stepper = subFilters().stepper();
		try {
			XnRegion sub;
			while ((sub = (XnRegion) stepper.fetch()) != null) {
				result = result.intersect(sub.complement());
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:67392:OrFilter methodsFor: 'operations'!
		{XnRegion} complement
			| result {XnRegion} |
			result _ Filter openFilter: self coordinateSpace.
			self subFilters stepper forEach: [ :sub {XnRegion} |
				result _ result intersect: sub complement].
			^result!
		*/
	}

	/**
	 * tell whether a region passes this filter
	 */
	public boolean match(XnRegion region) {
		Stepper stepper = subFilters().stepper();
		try {
			Filter sub;
			while ((sub = (Filter) stepper.fetch()) != null) {
				if (sub.match(region)) {
					return true;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return false;
		/*
		udanax-top.st:67402:OrFilter methodsFor: 'filtering'!
		{BooleanVar} match: region {XnRegion}
			"tell whether a region passes this filter"
			self subFilters stepper forEach: [ :sub {Filter} |
				(sub match: region)
					ifTrue: [^true]].
			^false!
		*/
	}

	/**
	 * return the simplest filter for looking at the children
	 */
	public Filter pass(Joint parent) {
		XnRegion result = Filter.closedFilter(coordinateSpace());
		Stepper stepper = subFilters().stepper();
		try {
			Filter sub;
			while ((sub = (Filter) stepper.fetch()) != null) {
				result = result.unionWith((sub.pass(parent)));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return (Filter) result;
		/*
		udanax-top.st:67409:OrFilter methodsFor: 'filtering'!
		{Filter} pass: parent {Joint}
			"return the simplest filter for looking at the children"
			| result {XnRegion} |
			result _ Filter closedFilter: self coordinateSpace.
			self subFilters stepper forEach: [ :sub {Filter} |
				result _ result unionWith: (sub pass: parent)].
			^result cast: Filter!
		*/
	}

	public ImmuSet subFilters() {
		return mySubFilters;
		/*
		udanax-top.st:67417:OrFilter methodsFor: 'filtering'!
		{ImmuSet of: Filter} subFilters
			^mySubFilters!
		*/
	}

	/**
	 * return self or other if one is clearly a subset of the other, else NULL
	 */
	public XnRegion fetchSpecialSubset(XnRegion other) {
		Filter filter = (Filter) other;
		XnRegion defaultRegion = this;
		Stepper stepper = subFilters().stepper();
		try {
			Filter subfilter;
			while ((subfilter = (Filter) stepper.fetch()) != null) {
				XnRegion a;
				XnRegion b;
				if (((a = subfilter.fetchSpecialSubset(filter))) == other) {
					return other;
				}
				if (((b = filter.fetchSpecialSubset(subfilter))) == other) {
					return other;
				}
				if (!(a == other || (b == other))) {
					defaultRegion = null;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return defaultRegion;
		/*
		udanax-top.st:67422:OrFilter methodsFor: 'protected: protected operations'!
		{XnRegion} fetchSpecialSubset: other {XnRegion}
			"return self or other if one is clearly a subset of the other, else NULL"
			| filter {Filter} defaultRegion {XnRegion} |
			filter _ other cast: Filter.
			defaultRegion _ self.
			self subFilters stepper forEach: [ :subfilter {Filter} | 
				| a {XnRegion} b {XnRegion} |
				((a _ subfilter fetchSpecialSubset: filter) basicCast: Heaper star) == other ifTrue: [^other].
				((b _ filter fetchSpecialSubset: subfilter) basicCast: Heaper star) == other ifTrue: [^other].
				((a basicCast: Heaper star) == other or: [(b basicCast: Heaper star) == other]) ifFalse: [defaultRegion _ NULL]].
			^defaultRegion!
		*/
	}

	public Stepper intersectedFilters() {
		return Stepper.itemStepper(this);
		/*
		udanax-top.st:67436:OrFilter methodsFor: 'enumerating'!
		{Stepper of: Filter} intersectedFilters
			^Stepper itemStepper: self!
		*/
	}

	public Stepper unionedFilters() {
		return mySubFilters.stepper();
		/*
		udanax-top.st:67440:OrFilter methodsFor: 'enumerating'!
		{Stepper of: Filter} unionedFilters
			^mySubFilters stepper!
		*/
	}

	public XnRegion baseRegion() {
		throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE_ENOUGH);
		/*
		udanax-top.st:67446:OrFilter methodsFor: 'accessing'!
		{XnRegion} baseRegion
			Heaper BLAST: #NotSimpleEnough.
			^NULL!
		*/
	}

	public XnRegion relevantRegion() {
		XnRegion result = filterSpace().baseSpace().emptyRegion();
		Stepper stepper = mySubFilters.stepper();
		try {
			Filter sub;
			while ((sub = (Filter) stepper.fetch()) != null) {
				result = result.unionWith(sub.relevantRegion());
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:67451:OrFilter methodsFor: 'accessing'!
		{XnRegion} relevantRegion
			| result {XnRegion} |
			result := self filterSpace baseSpace emptyRegion.
			mySubFilters stepper forEach: [ :sub {Filter} |
				result := result unionWith: sub relevantRegion].
			^result!
		*/
	}

	public OrFilter(Rcvr receiver) {
		super(receiver);
		mySubFilters = (ImmuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:67461:OrFilter methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			mySubFilters _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(mySubFilters);
		/*
		udanax-top.st:67465:OrFilter methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: mySubFilters.!
		*/
	}
}
