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
package info.dgjones.abora.white.collection.steppers;

import java.io.PrintWriter;

import info.dgjones.abora.white.collection.tables.MuArray;
import info.dgjones.abora.white.xpp.basic.Heaper;

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
