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

import java.io.ByteArrayOutputStream;
import java.io.PrintWriter;

public abstract class Heaper {


	//////////////////////////////////////////////
	// Constructors
	
	public Heaper() {
		super();
	}


	//////////////////////////////////////////////
	// Comparison and Hashing

	//TODO review whether we should be  connecting into java
	public boolean equals(Object other) {
		if (other instanceof Heaper) {
			Heaper o = (Heaper) other;
			return isEqual(o);
		} else {
			return super.equals(other);
		}
	}
	
	/**
	 * Return true if the two objects are equal.
	 */
	public abstract boolean isEqual(Heaper other);
	//TODO use equals?
	/*
	Xanadu-Xpp-Basic.st:285:Heaper methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		"Return true if the two objects are equal."
		self subclassResponsibility!
	*/

	/**
	 * The value returned does not change during the life of the object.
	 */
	public int hashForEqual() {
		//TODO use hashCode?
		return actualHashForEqual();
	}

	/**
	 * Defined by subclasses to produce the value returned by hashForEqual.
	 */
	protected int actualHashForEqual() {
		return System.identityHashCode(this);
	}


	//////////////////////////////////////////////
	// Printing
	
	/**
	 * This should rarely be overridden.  In Tofu, it prints ClassName(...),
	 * where ... is either produced by printInsideOn or is ??? if printInsideOn
	 * it not overridden.
	 */
	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print('(');
		try {
			printContentsOn(oo);
		} catch (RuntimeException e) {
			oo.print("***PRINT BLASTED***");
		}
		oo.print(')');
	}

	/**
	 * Subclasses override this method to customize their printing.
	 */
	public void printContentsOn(PrintWriter oo) {
		oo.print("????");
	}

	public String toString() {
		//TODO performance concerns over the choice of PrintWriter
		ByteArrayOutputStream outputStream = new ByteArrayOutputStream(); 
		PrintWriter printWriter = new PrintWriter(outputStream);
		printOn(printWriter);
		printWriter.flush();
		return outputStream.toString();
	}
}
