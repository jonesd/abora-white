/*
 * Abora hypertext system
 * Copyright 2003 David G Jones, david_jones@night.dircon.co.uk
 * 
 * Translated from Udanax-Gold source code: Copyright 1991 XOC, www.udanax.com
 */

package org.abora.white.collection.basic;

import org.abora.gold.java.missing.smalltalk.Stream;
import org.abora.gold.x.PrimSpec;
import org.abora.gold.xcvr.Rcvr;
import org.abora.gold.xcvr.Xmtr;
import org.abora.gold.xpp.basic.Heaper;

/**
 * @author jonesd
 */
public class Int32Array extends PrimIntArray {

//	Int32Array (Int32 count, TCSJ);

	protected Int32Array(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
		throw new UnsupportedOperationException();
	}

	protected Int32Array(int count, int[] buffer) {
		throw new UnsupportedOperationException();
	}

	  /** create an Int32Array filled with zeros */
	  public static Int32Array make(int count) {
	  	throw new UnsupportedOperationException();
	  }

	  /** create an Int32Array filled with the indicated data in 'from' */
	  public static Int32Array make(int size, PrimArray from, int sourceOffset, int count, int destOffset) {
	  	throw new UnsupportedOperationException();
	  }

	public static Int32Array make(int size, PrimArray from, int sourceOffset, int count) {
		return make(size, from, sourceOffset, count, 0);
	}

	public static Int32Array make(int size, PrimArray from, int sourceOffset) {
		return make(size, from, sourceOffset, -1);
	}

	public static Int32Array make(int size, PrimArray from) {
		return make(size, from, 0);
	}

	  /** create an Int32Array filled with the data at 'buffer' */
	  public static Int32Array make(int count, int[] buffer) {
	  	throw new UnsupportedOperationException();
	  }


	  /** Store a 32 bit signed integer value */
	  public void storeInt(int index, int value) {
	  	throw new UnsupportedOperationException();
	  }

	  /** Get a 32 bit signed actual integer value */
	  public int intAt(int index) {
	  	throw new UnsupportedOperationException();
	  }

	  public void storeInteger(int index, IntegerVar value) {
	  	throw new UnsupportedOperationException();
	  }

	  public IntegerVar integerAt(int index) {
	  	throw new UnsupportedOperationException();
	  }

	  public void storeValue(int index, Heaper value) {
	  	throw new UnsupportedOperationException();
	  }

	  public Heaper fetchValue(int index) {
	  	throw new UnsupportedOperationException();
	  }

	  public PrimSpec spec() {
	  	throw new UnsupportedOperationException();
	  }

	  public int bitCount() {
	  	throw new UnsupportedOperationException();
	  }

		  public void copyToBuffer(int[] buffer, int size, int count, int start) {
		  	throw new UnsupportedOperationException();
		  }
		  


	  private void receiveInt32Array(Rcvr rcvr) {
	  	throw new UnsupportedOperationException();
	  }

	  private void sendInt32Array(Xmtr xmtr) {
	  	throw new UnsupportedOperationException();
	  }


	  protected int compareData(int myStart, PrimDataArray other, int otherStart, int count) {
	  	throw new UnsupportedOperationException();
	  }

	  protected int signOfNonZeroAfter(int start) {
	  	throw new UnsupportedOperationException();
	  }

	  protected void addData(int myStart, PrimDataArray other, int otherStart, int count) {
	  	throw new UnsupportedOperationException();
	  }

	  protected void subtractData(int myStart, PrimDataArray other, int otherStart, int count) {
	  	throw new UnsupportedOperationException();
	  }

	  protected void printElementOn(int index, Stream oo) {
	  	throw new UnsupportedOperationException();
	  }

	  protected PrimArray makeNew(int size, PrimArray source, int sourceOffset, int count, int destOffset) {
	  	throw new UnsupportedOperationException();
	  }

}
