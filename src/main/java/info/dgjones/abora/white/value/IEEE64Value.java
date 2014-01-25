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
package info.dgjones.abora.white.value;


/**
 * Boxed Java double or IEEE64 value. 
 */
public class IEEE64Value extends PrimFloatValue {
	private final double value;
	
	private static final IEEE64Value ZERO = IEEE64Value.make(0.0);
	
	//////////////////////////////////////////////
	// Constructors
	
	protected IEEE64Value(double value) {
		super();
		this.value = value;
	}


	//////////////////////////////////////////////
	// Static Factory Methods

	public static IEEE64Value make(double value) {
		return new IEEE64Value(value);
	}

	public static IEEE64Value zero() {
		return ZERO;
	}

	//////////////////////////////////////////////
	// Conversions
	
	public float asIEEE32() {
		return (float)value;
	}

	public double asIEEE64() {
		return value;
	}
}
