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

import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.xpp.basic.Heaper;

/**
 * InstanceCache is intended to store a small number of frequently used objects with the
 * intent of reducing memory allocation traffic.
 */
public class InstanceCache extends Heaper {
	protected PtrArray myArray;
	protected int myTop;
	/*
	udanax-top.st:27691:
	Heaper subclass: #InstanceCache
		instanceVariableNames: '
			myArray {PtrArray}
			myTop {Int32}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-cache'!
	*/
	/*
	udanax-top.st:27697:
	InstanceCache comment:
	'InstanceCache is intended to store a small number of frequently used objects with the intent of reducing memory allocation traffic.'!
	*/
	/*
	udanax-top.st:27699:
	(InstanceCache getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/
	/*
	udanax-top.st:27740:
	InstanceCache class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:27743:
	(InstanceCache getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #EQ; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	protected InstanceCache(int size) {
		super();
		myArray = PtrArray.make(size);
		myTop = -1;
		/*
		udanax-top.st:27728:InstanceCache methodsFor: 'protected: create'!
		create: size {Int32}
			super create.
			myArray := PtrArray nulls: size.
			myTop := -1!
		*/
	}


	/////////////////////////////////////////////
	// Static Factory Methods

	public static InstanceCache make(int size) {
		return new InstanceCache(size);
		/*
		udanax-top.st:27748:InstanceCache class methodsFor: 'create'!
		make: size {Int32}
			^ self create: size!
		*/
	}


	/////////////////////////////////////////////
	// Accessing
	
	public Heaper fetch() {
		if (myTop >= 0) {
			Heaper result = myArray.fetch(myTop);
			myArray.store(myTop, null);
			myTop = myTop - 1;
			return result;
		} else {
			return null;
		}
		/*
		udanax-top.st:27704:InstanceCache methodsFor: 'accessing'!
		{Heaper} fetch
			myTop >= Int32Zero
				ifTrue: [
					| result {Heaper} |
					result := myArray fetch: myTop.
					myArray at: myTop store: NULL.
					myTop := myTop - 1.
					^ result]
				ifFalse: [
					^ NULL]!
		*/
	}

	public boolean store(Heaper object) {
		if (myTop < (myArray.count() - 1)) {
			myTop = myTop + 1;
			//TODO UG destructs/suspends stored object - we dont!!!
//			object.destruct();
//			new SuspendedHeaper(object);
			myArray.store(myTop, object);
			return true;
		} else {
			return false;
		}
		/*
		udanax-top.st:27715:InstanceCache methodsFor: 'accessing'!
		{BooleanVar} store: object {Heaper}
			myTop < (myArray count - 1)
				ifTrue: [
					myTop := myTop + 1.
					object destruct.
					(SuspendedHeaper new.Become: object) create.
					myArray at: myTop store: object.
					^ true]
				ifFalse: [
					^ false]!
		*/
	}


	/////////////////////////////////////////////
	// Comparing and Hashing
	
	public int actualHashForEqual() {
		return System.identityHashCode(this);
//		TODOreturn asOop();
		/*
		udanax-top.st:27735:InstanceCache methodsFor: 'generated:'!
		actualHashForEqual ^self asOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:27737:InstanceCache methodsFor: 'generated:'!
		isEqual: other ^self == other!
		*/
	}
}
