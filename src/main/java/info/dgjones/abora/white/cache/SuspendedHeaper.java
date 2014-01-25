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
package info.dgjones.abora.white.cache;

import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Heapers cached to avoid memory allocation overhead are kept as SuspendedHeapers to reduce
 * GC overhead.
 */
public class SuspendedHeaper extends Heaper {
	/*
	udanax-top.st:56449:
	Heaper subclass: #SuspendedHeaper
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-cache'!
	*/
	/*
	udanax-top.st:56453:
	SuspendedHeaper comment:
	'Heapers cached to avoid memory allocation overhead are kept as SuspendedHeapers to reduce GC overhead.'!
	*/
	/*
	udanax-top.st:56455:
	(SuspendedHeaper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; add: #NOT.A.TYPE; yourself)!
	*/

	public SuspendedHeaper() {
		super();
		/*
		udanax-top.st:56460:SuspendedHeaper methodsFor: 'creation'!
		{INLINE} create
			super create!
		*/
	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		return asOop();
		/*
		udanax-top.st:56465:SuspendedHeaper methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:56467:SuspendedHeaper methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}
}
