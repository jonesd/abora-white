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
