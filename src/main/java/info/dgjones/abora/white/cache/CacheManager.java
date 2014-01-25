/**
 * The MIT License
 * Copyright (c) 2003 David G Jones
 *
 * Permission is hereby granted, free of charge, to any person obtaining a copy
 * of this software and associated documentation files (the "Software"), to deal
 * in the Software without restriction, including without limitation the rights
 * to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 * copies of the Software, and to permit persons to whom the Software is
 * furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included in
 * all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 * IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 * AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 * OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 * THE SOFTWARE.
 */
package info.dgjones.abora.white.cache;

import info.dgjones.abora.white.xpp.basic.Heaper;

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
