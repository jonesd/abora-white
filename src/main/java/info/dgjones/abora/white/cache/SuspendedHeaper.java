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
