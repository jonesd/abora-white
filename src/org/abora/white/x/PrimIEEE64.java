package org.abora.white.x;

/**
 * Boxed Java double or IEEE64 value. 
 */
public class PrimIEEE64 extends PrimFloatValue {
	private double value;
	
	protected PrimIEEE64(double value) {
		super();
		this.value = value;
	}

	public static PrimIEEE64 make(double value) {
		return new PrimIEEE64(value);
	}

	public float asIEEE32() {
		return (float)value;
	}

	public double asIEEE64() {
		return value;
	}

}
