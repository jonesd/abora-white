/*
 * Abora White: Abora hypertext system based on Udanax-Gold
 * Copyright 2003 David G Jones, david_jones@night. dircon.co.uk
 * Parts translated from Udanax-Gold source: Copyright 1991 XOC, www.udanax. com
 */

package org.abora.white.xpp.basic;

public abstract class Heaper {

	public Heaper() {
		super();
	}

	/**
	 * Return true if the two objects are equal.
	 */
	public abstract boolean isEqual(Heaper other);
	/*
	Xanadu-Xpp-Basic.st:285:Heaper methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		"Return true if the two objects are equal."
		self subclassResponsibility!
	*/

}
