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
package org.abora.white.collection.steppers;

import java.io.PrintWriter;

import org.abora.white.collection.tables.MuArray;
import org.abora.white.xpp.basic.Heaper;

/**
 * Consider this class''s public status as obsolete.  Eventually This class will either be
 * private of get retired.
 */
public abstract class TableAccumulator extends Accumulator {
	/*
	udanax-top.st:12379:
	Accumulator subclass: #TableAccumulator
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Steppers'!
	*/
	/*
	udanax-top.st:12383:
	TableAccumulator comment:
	'Consider this class''s public status as obsolete.  Eventually This class will either be private of get retired.'!
	*/
	/*
	udanax-top.st:12385:
	(TableAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:12410:
	TableAccumulator class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:12413:
	(TableAccumulator getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/**
	 * Add elem to the internal table.
	 */
	public abstract void step(Heaper elem);
	/*
	udanax-top.st:12390:TableAccumulator methodsFor: 'deferred operations'!
	{void} step: elem {Heaper}
		"Add elem to the internal table."
		self subclassResponsibility!
	*/

	/**
	 * Return the accumulated table.
	 */
	public abstract Heaper value();
	/*
	udanax-top.st:12394:TableAccumulator methodsFor: 'deferred operations'!
	{Heaper} value
		"Return the accumulated table."
		self subclassResponsibility!
	*/

	/**
	 * Should this copy the array?
	 */
	public abstract Accumulator copy();
	/*
	udanax-top.st:12400:TableAccumulator methodsFor: 'deferred create'!
	{Accumulator} copy
		"Should this copy the array?"
		self subclassResponsibility!
	*/

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print(" on ");
		oo.print(value());
		/*
		udanax-top.st:12406:TableAccumulator methodsFor: 'printing'!
		{void} printOn: oo {ostream reference} 
			oo << self getCategory name << ' on ' << self value!
		*/
	}

	/**
	 * Returns an Accumulator which will produce an MuArray of the elements
	 * accumulated into it in order of accumulation. See MuArray. Equivalent to
	 * 'arrayAccumulator()'. Eventually either he or I should be declared obsolete. INLINE
	 */
	public static TableAccumulator make() {
		return MuArray.arrayAccumulator();
		/*
		udanax-top.st:12418:TableAccumulator class methodsFor: 'pseudoConstructors'!
		{TableAccumulator} make
			"Returns an Accumulator which will produce an MuArray of the elements 
			accumulated into it in order of accumulation. See MuArray. Equivalent to 
			'arrayAccumulator()'. Eventually either he or I should be declared obsolete. INLINE"
			^MuArray arrayAccumulator!
		*/
	}
}
