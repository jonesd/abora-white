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
package org.abora.white.spaces.basic;

public class OrderEnum {
	private final String name;
	
	private OrderEnum(String name) {
		this.name = name;
	}
	
	public String toString() {
		return name;
	}
	
	public static final OrderEnum EQUAL = new OrderEnum("Equal");
	public static final OrderEnum GREATER = new OrderEnum("Greater");
	public static final OrderEnum LESS = new OrderEnum("Less");
	public static final OrderEnum INCOMPARABLE = new OrderEnum("Incomparable");
}
