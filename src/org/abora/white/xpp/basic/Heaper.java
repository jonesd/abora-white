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

import java.io.PrintStream;

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

	/**
	 * The value returned does not change during the life of the object.
	 */
	public int hashForEqual() {
		throw new UnsupportedOperationException();
	}

	/**
	 * Defined by subclasses to produce the value returned by hashForEqual.
	 */
	protected int actualHashForEqual() {
		throw new UnsupportedOperationException();
	}

	/**
	 * This should rarely be overridden.  In Tofu, it prints ClassName(...),
	 * where ... is either produced by printInsideOn or is ??? if printInsideOn
	 * it not overridden.
	 */
	public void printOn(PrintStream oo) {
		oo.print(getClass().getName());
		oo.print('(');
		try {
			printContentsOn(oo);
		} catch (RuntimeException e) {
			oo.print("***PRINT BLASTED***");
		}
		oo.print(')');
		//		void Heaper::printOn (ostream& oo)
		//		{
		//			oo << this->getCategory()->name() << "(";
		//			do {
		//				INSTALL_SHIELD(pr);
		//				SHIELD_UP_BEGIN(pr,AllBlastsFilter) {
		//					oo << "***PRINT BLASTED***";
		//					break;
		//				} SHIELD_UP_END(pr);
		//				this->printContentsOn(oo);
		//			} while (FALSE);
		//			oo << ")";
		//		}
	}

	/**
	 * Subclasses override this method to customize their printing.
	 */
	public void printContentsOn(PrintStream oo) {
		oo.print("????");
		//		void Heaper::printContentsOn (ostream& oo)
		//		{
		//			oo << "????";
		//		}
	}

}
