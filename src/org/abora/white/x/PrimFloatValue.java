package org.abora.white.x;

import org.abora.white.xpp.basic.Heaper;

/**
 * @author jonesd
 */
public abstract class PrimFloatValue extends PrimValue {

	protected PrimFloatValue() {
		super();
	}

	/**
	 * The value as an IEEE 32-bit floating point number.
	 * May not be possible if conversion from subclass to IEEE type is not available.
	 */
	public abstract float asIEEE32()
	/*
	all.st:36754:PrimFloatValue methodsFor: 'accessing'!
	{IEEE32} asIEEE32
		"The value as an IEEE 32-bit floating point number.
		May not be possible if conversion from subclass to IEEE type is not available."
	
		self subclassResponsibility!
	*/
	;
	
	public abstract double asIEEE64();

	//TODO what does ug do?
	public boolean isEqual(Heaper other) {
		if (other instanceof PrimFloatValue) {
			PrimFloatValue otherFloat = (PrimFloatValue) other;
			return this.asIEEE64() == otherFloat.asIEEE64();
		} else {
			return false;
		}
	}

}
