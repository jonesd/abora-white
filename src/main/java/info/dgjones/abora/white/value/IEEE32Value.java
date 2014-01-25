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
 * Boxed Java float or IEEE32 value. 
 */
public class IEEE32Value extends PrimFloatValue {

	private float value;

	//////////////////////////////////////////////
	// Constructors
	
	protected IEEE32Value(float value) {
		super();
		this.value = value;
	}


	//////////////////////////////////////////////
	// Static Factory Method

	public static IEEE32Value make(float f) {
		return new IEEE32Value(f);
	}


	//////////////////////////////////////////////
	// Conversions

	public float asIEEE32() {
		return value;
	}

	public double asIEEE64() {
		return value;
	}
}
