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
package org.abora.white.cache;

import org.abora.white.xpp.basic.Heaper;

public abstract class CacheManager extends Heaper {
	/*
	udanax-top.st:13065:
	Heaper subclass: #CacheManager
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-cache'!
	*/
	/*
	udanax-top.st:13069:
	(CacheManager getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/**
	 * Return the value associated with the key, if any.
	 */
	public abstract Heaper fetch(Heaper key);
	/*
	udanax-top.st:13074:CacheManager methodsFor: 'accessing'!
	{Heaper | NULL} fetch: key {Heaper}
		"Return the value associated with the key, if any."
		
		self subclassResponsibility!
	*/

	/**
	 * Does te cach contain something at the given key?
	 */
	public abstract boolean hasMember(Heaper key);
	/* Should the key be a Heaper or a Position? */
	/*
	udanax-top.st:13079:CacheManager methodsFor: 'accessing'!
	{BooleanVar} hasMember: key {Heaper}
		"Does te cach contain something at the given key?"
		
		"Should the key be a Heaper or a Position?"
		
		self subclassResponsibility!
	*/

	/**
	 * Remove the cached association with key.  Return true if the cache contained something at
	 * that key.
	 */
	public abstract boolean wipe(Heaper key);
	/*
	udanax-top.st:13086:CacheManager methodsFor: 'accessing'!
	{BooleanVar} wipe: key {Heaper}
		"Remove the cached association with key.  Return true if the cache contained something at that key."
		
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
//	TODO	return Heaper.takeOop();
		/*
		udanax-top.st:13093:CacheManager methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}
}
