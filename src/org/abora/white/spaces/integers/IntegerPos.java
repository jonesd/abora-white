/*
 * Abora-White
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Based on the Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 * 
 * Id:
 */
package org.abora.white.spaces.integers;

import java.io.PrintWriter;

import org.abora.white.spaces.basic.Position;

public class IntegerPos extends Position {
	private int value;

	private static final IntegerPos TheZero = new IntegerPos(0);

	public IntegerPos(int value) {
		super();
		this.value = value;
	}

	/**
	 * Return a canonical boxed zero. 
	 */
	public static IntegerPos zero() {
		return TheZero;
		/*
		udanax-top.st:31985:IntegerPos class methodsFor: 'pseudo constructors'!
		{IntegerPos INLINE} zero
			"Box an integer. See XuInteger class comment. you can also create an 
			integer in smalltalk by sending the integer message to a Smalltalk integer.
			This should return the canonical zero eventually."
			^IntegerPos make: IntegerVarZero!
		*/
	}

	public int asInt() {
		return value;
	}

	public void printOn(PrintWriter oo) {
		oo.print("I(");
		oo.print(value);
		oo.print(')');
		/*
		udanax-top.st:31951:IntegerPos methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << 'I(' << myValue << ')'!
		*/
	}

}
