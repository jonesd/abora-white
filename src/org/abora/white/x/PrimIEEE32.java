package org.abora.white.x;

import org.abora.white.xpp.basic.Heaper;

/**
 * Boxed Java float or IEEE32 value. 
 */
public class PrimIEEE32 extends PrimFloatValue {

	private float value;

	protected PrimIEEE32(float value) {
		super();
		this.value = value;
	}

	public static Heaper make(float f) {
		return new PrimIEEE32(f);
	}

	/**
	 * The value as an IEEE 32-bit floating point number
	 */
	public float asIEEE32() {
		return value;
		/*
		all.st:36858:PrimIEEE32 methodsFor: 'accessing'!
		{IEEE32} asIEEE32
			"The value as an IEEE 32-bit floating point number"
			^ myValue!
		*/
	}

	public double asIEEE64() {
		return value;
	}
}
