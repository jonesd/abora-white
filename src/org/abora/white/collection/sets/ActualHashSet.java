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
package org.abora.white.collection.sets;

import java.io.PrintWriter;

import org.abora.white.collection.arrays.Int32Array;
import org.abora.white.collection.arrays.PtrArray;
import org.abora.white.collection.arrays.SharedPtrArray;
import org.abora.white.collection.steppers.HashSetStepper;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.tabtool.LPPrimeSizeProvider;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class ActualHashSet extends HashSet {
	protected Int32Array myHashValues;
	protected SharedPtrArray myHashEntries;
	protected int myTally;
	//	protected static IntegerVar AddOver;
	//	protected static Array AddTallys;
	//	protected static IntegerVar DeleteOver;
	//	protected static Array DeleteTallys;
	//	protected static IntegerVar NewSetCount;
	//	protected static IntegerVar SetKillCount;
	//	protected static IntegerVar StepperCount;
	//	protected static IntegerVar StepperOver;
	//	protected static Array StepperTally;
	//	protected static IntegerVar TestOver;
	//	protected static Array TestTallys;
	/*
	udanax-top.st:46378:
	HashSet subclass: #ActualHashSet
		instanceVariableNames: '
			myHashValues {UInt32Array NOCOPY}
			myHashEntries {SharedPtrArray NOCOPY}
			myTally {Int32}'
		classVariableNames: '
			AddOver {IntegerVar smalltalk} 
			AddTallys {Array smalltalk} 
			DeleteOver {IntegerVar smalltalk} 
			DeleteTallys {Array smalltalk} 
			NewSetCount {IntegerVar smalltalk} 
			SetKillCount {IntegerVar smalltalk} 
			StepperCount {IntegerVar smalltalk} 
			StepperOver {IntegerVar smalltalk} 
			StepperTally {Array smalltalk} 
			TestOver {IntegerVar smalltalk} 
			TestTallys {Array smalltalk} '
		poolDictionaries: ''
		category: 'Xanadu-Collection-Sets'!
	*/
	/*
	udanax-top.st:46396:
	(ActualHashSet getOrMakeCxxClassDescription)
		friends:
	'/- friends for class ActualHashSet -/
	friend class HashSetTester;';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:46755:
	ActualHashSet class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:46758:
	(ActualHashSet getOrMakeCxxClassDescription)
		friends:
	'/- friends for class ActualHashSet -/
	friend class HashSetTester;';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected ActualHashSet(int newTally, SharedPtrArray entries) {
		super();
		myTally = newTally;
		myHashValues = Int32Array.make(entries.count());
		myHashEntries = entries;
		myHashEntries.shareMore();
		//		/* >>> smalltalkOnly */
		//		NewSetCount = NewSetCount + 1;
		//		/* <<< smalltalkOnly */
		/*
		udanax-top.st:46432:ActualHashSet methodsFor: 'protected: creation'!
		create: newTally {Int32} with: entries {SharedPtrArray}
			super create.
			myTally _ newTally.
			myHashValues _ UInt32Array make: entries count.
			myHashEntries _ entries.
			myHashEntries shareMore.
			[NewSetCount _ NewSetCount + 1] smalltalkOnly.!
		*/
	}

	protected ActualHashSet(int newTally, Int32Array hashValues, SharedPtrArray entries) {
		super();
		myTally = newTally;
		myHashValues = hashValues;
		myHashEntries = entries;
		myHashEntries.shareMore();
		//		/* >>> smalltalkOnly */
		//		NewSetCount = NewSetCount + 1;
		//		/* <<< smalltalkOnly */
		/*
		udanax-top.st:46440:ActualHashSet methodsFor: 'protected: creation'!
		create: newTally {Int32} with: hashValues {UInt32Array} with: entries {SharedPtrArray}
			super create.
			myTally _ newTally.
			myHashValues _ hashValues.
			myHashEntries _ entries.
			myHashEntries shareMore.
			[NewSetCount _ NewSetCount + 1] smalltalkOnly.!
		*/
	}

	/////////////////////////////////////////////
	// Testing

	public int contentsHash() {
		int hashResult;
		hashResult = 0;
		for (int idx = 0; idx < myHashEntries.count(); idx++) {
			if (myHashEntries.fetch(idx) != null) {
				hashResult = hashResult + (myHashValues.int32At(idx));
			}
		}
		return hashResult;
		/*
		udanax-top.st:46404:ActualHashSet methodsFor: 'testing'!
		{UInt32} contentsHash
			| hashResult {UInt32} |
			hashResult _ UInt32Zero.
			UInt32Zero almostTo: myHashEntries count do: [:idx {UInt32} | 
				(myHashEntries fetch: idx) ~~ NULL 
					ifTrue: [hashResult _ hashResult + (myHashValues uIntAt: idx)]].
			^hashResult!
		*/
	}

	public IntegerValue count() {
		return IntegerValue.make(myTally);
		/*
		udanax-top.st:46414:ActualHashSet methodsFor: 'accessing'!
		{IntegerVar} count
			
			^ myTally!
		*/
	}

	public boolean hasMember(Heaper someone) {
		//		/* >>> smalltalkOnly */
		//		ActualHashSet.countTest(myTally);
		//		/* <<< smalltalkOnly */
		return (hashFind(someone)) >= 0;
		/*
		udanax-top.st:46418:ActualHashSet methodsFor: 'accessing'!
		{BooleanVar} hasMember: someone {Heaper}
			[self class countTest: myTally] smalltalkOnly.
			^(self hashFind: someone) >= Int32Zero!
		*/
	}

	public boolean isEmpty() {
		return myTally == 0;
		/*
		udanax-top.st:46422:ActualHashSet methodsFor: 'accessing'!
		{BooleanVar} isEmpty
			^ myTally == UInt32Zero!
		*/
	}

	public ScruSet copy() {
		return new ActualHashSet(myTally, myHashValues, myHashEntries);
		/*
		udanax-top.st:46427:ActualHashSet methodsFor: 'creation'!
		{ScruSet} copy
			^ActualHashSet create: myTally with: myHashValues with: myHashEntries!
		*/
	}

	public void destruct() {
		myHashEntries.shareLess();
		super.destruct();
		/*
		udanax-top.st:46448:ActualHashSet methodsFor: 'protected: creation'!
		{void} destruct
			myHashEntries shareLess.
			super destruct!
		*/
	}

	public Stepper stepper() {
		//		/* >>> smalltalkOnly */
		//		StepperCount = StepperCount + 1;
		//		ActualHashSet.countStepper(myTally);
		//		/* <<< smalltalkOnly */
		return HashSetStepper.make(myHashEntries);
		/*
		udanax-top.st:46454:ActualHashSet methodsFor: 'enumerating'!
		{Stepper} stepper
			[StepperCount _ StepperCount + 1.
			self class countStepper: myTally] smalltalkOnly.
			^ HashSetStepper make: myHashEntries!
		*/
	}

	public Heaper theOne() {
		if (myTally != 1) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
		}
		for (int i = 0; i < myHashEntries.count(); i++) {
			if (myHashEntries.fetch(i) != null) {
				return myHashEntries.fetch(i);
			}
		}
		return null;
		/*
		udanax-top.st:46459:ActualHashSet methodsFor: 'enumerating'!
		{Heaper} theOne
			myTally ~~ 1 ifTrue: [Heaper BLAST: #NotOneElement].
			Int32Zero almostTo: myHashEntries count do: [:i {Int32} |
				(myHashEntries fetch: i) ~~ NULL ifTrue: [^myHashEntries fetch: i]].
			^NULL!
		*/
	}

	/**
	 * union equivalent
	 */
	public void storeAll(ScruSet other) {
		boolean haveStored = false;
		Stepper stepper = other.stepper();
		try {
			Heaper elem;
			while ((elem = (Heaper) stepper.fetch()) != null) {
				if (hashFind(elem) < 0) {
					if (!haveStored) {
						haveStored = true;
						//						/* >>> smalltalkOnly */
						//						ActualHashSet.countAdd(myTally);
						//						/* <<< smalltalkOnly */
						checkSize(other.count().asInt32());
					}
					hashStore(elem, myHashValues, myHashEntries);
					myTally = myTally + 1;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		/*
		udanax-top.st:46467:ActualHashSet methodsFor: 'operations'!
		{void} storeAll: other {ScruSet} 
			"union equivalent"
			| haveStored {BooleanVar} |
			haveStored _ false.
			other stepper forEach: [:elem {Heaper wimpy} | 
				(self hashFind: elem) < Int32Zero ifTrue:
					[haveStored ifFalse:
						[haveStored _ true.
						[self class countAdd: myTally] smalltalkOnly.
						self checkSize: other count DOTasLong].
					self hashStore: elem with: myHashValues with: myHashEntries.
					myTally _ myTally + 1]].!
		*/
	}

	/**
	 * Sort of minus.  Wipe from myself all elements from other.
	 * Turn myself into my current self minus other.
	 */
	public void wipeAll(ScruSet other) {
		/* Maintainance note: this duplicates some code in wipe: for efficiency */
		int loc;
		if (myTally == 0) {
			return;
		}
		boolean haveWritten = false;
		Stepper stepper = other.stepper();
		try {
			Heaper elem;
			while ((elem = (Heaper) stepper.fetch()) != null) {
				if ((loc = hashFind(elem)) >= 0) {
					if (!haveWritten) {
						aboutToWrite();
						haveWritten = true;
						//					/* >>> smalltalkOnly */
						//					ActualHashSet.countDelete(myTally);
						//					/* <<< smalltalkOnly */
					}
					hashRemove(loc);
					myTally = myTally - 1;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		/*
		udanax-top.st:46480:ActualHashSet methodsFor: 'operations'!
		{void} wipeAll: other {ScruSet} 
			"Sort of minus.  Wipe from myself all elements from other.
			Turn myself into my current self minus other."
			"Maintainance note: this duplicates some code in wipe: for efficiency"
			| loc {Int32} haveWritten {BooleanVar} |
			myTally = UInt32Zero ifTrue: [^ VOID].
			haveWritten _ false.
			other stepper forEach: [:elem {Heaper wimpy} |
				(loc _ self hashFind: elem) >= Int32Zero ifTrue:
					[haveWritten ifFalse:
						[self aboutToWrite.
						haveWritten _ true.
						[self class countDelete: myTally] smalltalkOnly.].
					self hashRemove: (loc basicCast: UInt32).
					myTally _ myTally - 1]]!
		*/
	}

	public void introduce(Heaper anElement) {
		//		/* >>> smalltalkOnly */
		//		ActualHashSet.countAdd(myTally);
		//		/* <<< smalltalkOnly */
		checkSize(1);
		if (hashFind(anElement) >= 0) {
			throw new AboraRuntimeException(AboraRuntimeException.ALREADY_IN_SET);
		} else {
			hashStore(anElement, myHashValues, myHashEntries);
			myTally = myTally + 1;
		}
		/*
		udanax-top.st:46499:ActualHashSet methodsFor: 'adding-removing'!
		{void} introduce: anElement {Heaper}
			[self class countAdd: myTally] smalltalkOnly.
			self checkSize: 1.
			(self hashFind: anElement) >= Int32Zero
				ifTrue: [Heaper BLAST: #AlreadyInSet]
				ifFalse: [self hashStore: anElement with: (myHashValues basicCast: UInt32Array) with: (myHashEntries basicCast: PtrArray).
					myTally _ myTally + 1]!
		*/
	}

	public void remove(Heaper anElement) {
		int loc;
		//		/* >>> smalltalkOnly */
		//		ActualHashSet.countDelete(myTally);
		//		/* <<< smalltalkOnly */
		aboutToWrite();
		if ((loc = hashFind(anElement)) >= 0) {
			hashRemove(loc);
			myTally = myTally - 1;
		} else {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_SET);
		}
		/*
		udanax-top.st:46507:ActualHashSet methodsFor: 'adding-removing'!
		{void} remove: anElement {Heaper}
			| loc {Int32} |
			[self class countDelete: myTally] smalltalkOnly.
			self aboutToWrite.
			(loc _ self hashFind: anElement) >= Int32Zero
				ifTrue: [self hashRemove: (loc basicCast: UInt32).
					myTally _ myTally - 1]
				ifFalse: [Heaper BLAST: #NotInSet]!
		*/
	}

	/**
	 * maintainance note: storeAll: has a copy of the code starting at self hashFind:... for
	 * efficiency.
	 */
	public void store(Heaper anElement) {
		if (hashFind(anElement) < 0) {
			checkSize(1);
			hashStore(anElement, myHashValues, myHashEntries);
			//			/* >>> smalltalkOnly */
			//			ActualHashSet.countAdd(myTally);
			//			/* <<< smalltalkOnly */
			myTally = myTally + 1;
		}
		/*
		udanax-top.st:46516:ActualHashSet methodsFor: 'adding-removing'!
		{void} store: anElement {Heaper}
			"maintainance note: storeAll: has a copy of the code starting at self hashFind:... for efficiency."
			(self hashFind: anElement) < Int32Zero ifTrue:
				[self checkSize: 1.
				self hashStore: anElement with: (myHashValues basicCast: UInt32Array) with: (myHashEntries basicCast: PtrArray).
				[self class countAdd: myTally] smalltalkOnly.
				myTally _ myTally + 1]!
		*/
	}

	public void wipe(Heaper anElement) {
		int loc;
		if (myTally == 0) {
			return;
		}
		if ((loc = hashFind(anElement)) >= 0) {
			//			/* >>> smalltalkOnly */
			//			ActualHashSet.countDelete(myTally);
			//			/* <<< smalltalkOnly */
			aboutToWrite();
			hashRemove(loc);
			myTally = myTally - 1;
		}
		/*
		udanax-top.st:46524:ActualHashSet methodsFor: 'adding-removing'!
		{void} wipe: anElement {Heaper}
			| loc {Int32} |
			myTally = UInt32Zero ifTrue: [^ VOID].
			(loc _ self hashFind: anElement) >= Int32Zero ifTrue:
				[[self class countDelete: myTally] smalltalkOnly.
				self aboutToWrite.
				self hashRemove: (loc basicCast: UInt32).
				myTally _ myTally - 1]!
		*/
	}

	/**
	 * If my contents are shared, and I'm about to change them, make a copy of them.
	 */
	public void aboutToWrite() {
		if (myHashEntries.shareCount() > 1) {
			actualAboutToWrite();
		}
		/*
		udanax-top.st:46535:ActualHashSet methodsFor: 'private: housekeeping'!
		{void INLINE} aboutToWrite
			"If my contents are shared, and I'm about to change them, make a copy of them."
			myHashEntries shareCount > 1 ifTrue:
				[self actualAboutToWrite]!
		*/
	}

	public void actualAboutToWrite() {
		Int32Array newValues = (Int32Array) myHashValues.copy();
		SharedPtrArray newEntries = (SharedPtrArray) myHashEntries.copy();
		myHashEntries.shareLess();
		myHashValues = newValues;
		myHashEntries = newEntries;
		myHashEntries.shareMore();
		/*
		udanax-top.st:46540:ActualHashSet methodsFor: 'private: housekeeping'!
		{void} actualAboutToWrite
			| newValues {UInt32Array} newEntries {SharedPtrArray}  |
			newValues _ myHashValues copy cast: UInt32Array.
			newEntries _ myHashEntries copy cast: SharedPtrArray.
			myHashEntries shareLess.
			myHashValues _ newValues.
			myHashEntries _ newEntries.
			myHashEntries shareMore!
		*/
	}

	public void checkSize(int byAmount) {
		Heaper he;
		/* Leave a third of free space. */
		if (((myTally + byAmount) * 5) >> 2 < myHashEntries.count()) {
			aboutToWrite();
			return;
		}
		int newSize = LPPrimeSizeProvider.make().uInt32PrimeAfter(((myHashValues.count() * 2) + byAmount));
		Int32Array newValues = Int32Array.make(newSize);
		SharedPtrArray newEntries = (SharedPtrArray) SharedPtrArray.make(newSize);
		for (int from = 0; from < myHashValues.count(); from++) {
			if ((he = myHashEntries.fetch(from)) != null) {
				hashStore(he, newValues, newEntries);
			}
		}
		if (myHashEntries.shareCount() > 1) {
			myHashEntries.shareLess();
		} else {
			myHashValues.destroy();
			myHashEntries.destroy();
		}
		myHashValues = newValues;
		myHashEntries = newEntries;
		myHashEntries.shareMore();
		/*
		udanax-top.st:46550:ActualHashSet methodsFor: 'private: housekeeping'!
		{void} checkSize: byAmount {Int32}
			| newSize {Int32} newValues {UInt32Array} newEntries {SharedPtrArray} he {Heaper wimpy} |
			
			"Leave a third of free space."
			(((myTally + byAmount) * 5) bitShiftRight: 2) < myHashEntries count ifTrue:
				[self aboutToWrite.
				^VOID].
			newSize _ LPPrimeSizeProvider make uInt32PrimeAfter: ((myHashValues count * 2) + byAmount).
			newValues _ UInt32Array make: newSize.
			newEntries _ SharedPtrArray make: newSize.
			Int32Zero almostTo: myHashValues count do: [:from {Int32 register} |
				(he _ myHashEntries fetch: from) ~~ NULL 
					ifTrue: [self hashStore: he with: newValues with: newEntries]].
			myHashEntries shareCount > 1
				ifTrue: [myHashEntries shareLess]
				ifFalse: [myHashValues destroy.
					myHashEntries destroy].
			myHashValues _ newValues.
			myHashEntries _ newEntries.
			myHashEntries shareMore!
		*/
	}

	public int distanceFromHome(int loc, int home, int modulus) {
		int dist;
		/* >>> smalltalkOnly */
		//TODOreturn (loc - home) % modulus;
		/* <<< smalltalkOnly */
		/* alternate coding if modulus doesn't handle negatives the same as smalltalk 
				(positive remainder only) */

		dist = (loc - home);
		if (dist < 0) {
			dist = dist + modulus;
		}
		return dist;
		/*
		udanax-top.st:46573:ActualHashSet methodsFor: 'private: housekeeping'!
		{Int32} distanceFromHome: loc {UInt32} with: home {UInt32} with: modulus {UInt32}
			| dist {Int32} |
			[^ (loc - home) \\ modulus] smalltalkOnly.
			"alternate coding if modulus doesn't handle negatives the same as smalltalk 
				(positive remainder only)"
			
			[dist _ (loc - home).
			dist < Int32Zero ifTrue: [dist _ dist + modulus].
			^ dist] translateOnly!
		*/
	}

	/**
	 * Starting at the item's preferred location and iterating (not recurring!!) around the set's
	 * storage while the slots we're examining are occupied...
	 * If the current slot's occupant is the target item, return a hit
	 * if the current occupant is closer to it's preferred location, return a miss.
	 * If we've gone all the way around, return a miss.
	 */
	public int hashFind(Heaper item) {
		int current;
		Heaper currentEntry;
		int currentHome;
		int tSize = myHashValues.count();
		int targetValue = item.hashForEqual();
		int targetHome = current = targetValue % tSize;
		while ((currentEntry = myHashEntries.fetch(current)) != null) {
			int currentValue = myHashValues.int32At(current);
			if (currentValue == targetValue) {
				if (currentEntry.isEqual(item)) {
					return current;
				}
			}
			/* Found it. */
			currentHome = currentValue % tSize;
			if (distanceFromHome(current, targetHome, tSize) > (distanceFromHome(current, currentHome, tSize))) {
				return -1;
			}
			/* Would have seen it by now. */
			current = current + 1 % tSize;
			if (current == targetHome) {
				return -1;
			}
			/* All the way around. */;
		}
		return -1
		/* Found an empty slot. */;
		/*
		udanax-top.st:46586:ActualHashSet methodsFor: 'private: hash resolution'!
		{Int32} hashFind: item {Heaper}
			"Starting at the item's preferred location and iterating (not recurring!!) around the set's
			 storage while the slots we're examining are occupied...
			   If the current slot's occupant is the target item, return a hit 
			   if the current occupant is closer to it's preferred location, return a miss.
			   If we've gone all the way around, return a miss."
			
			| tSize {UInt32} current {UInt32}
			 currentValue {UInt32} currentEntry {Heaper} currentHome {UInt32}
			 targetValue {UInt32} targetHome {UInt32} |
			 
			tSize _ myHashValues count.
			targetValue _ item hashForEqual.
			targetHome _ current _ targetValue \\ tSize.
			
			[(currentEntry _ myHashEntries fetch: current) ~~ NULL]
				whileTrue: [
					currentValue _ myHashValues uIntAt: current.
					
					currentValue = targetValue ifTrue:
						[(currentEntry isEqual: item) ifTrue: [^current]].						"Found it."
						
					currentHome _ currentValue \\ tSize.
					(self distanceFromHome: current with: targetHome with: tSize) >
					(self distanceFromHome: current with: currentHome with: tSize)
						ifTrue: [^ -1].							"Would have seen it by now."
						
					current _ current + 1 \\ tSize.
					current = targetHome ifTrue: [^ -1].		"All the way around."
			].
			^ -1												"Found an empty slot."!
		*/
	}

	/**
	 * Remove the indicated item from the set.
	 * Iteratively (not recursively!!) move other items up until one is NULL or happier where it
	 * is.
	 */
	public void hashRemove(int from) {
		int next;
		int nextValue;
		Heaper nextEntry;
		int current = from;
		int tSize = myHashValues.count();
		while ((nextEntry = myHashEntries.fetch((next = current + 1 % tSize))) != null
			&& (((nextValue = myHashValues.int32At(next)) % tSize) != next)) {
			myHashEntries.store(current, nextEntry);
			myHashValues.storeInt32(current, nextValue);
			current = next;
		}
		myHashEntries.store(current, null);
		myHashValues.storeInt32(current, 0);
		/*
		udanax-top.st:46620:ActualHashSet methodsFor: 'private: hash resolution'!
		{void} hashRemove: from {UInt32} 
			"Remove the indicated item from the set.
			 Iteratively (not recursively!!) move other items up until one is NULL or happier where it is."
			| tSize {UInt32} current {UInt32} next {UInt32} nextValue {UInt32} nextEntry {Heaper} |
			
			current _ from.
			tSize _ myHashValues count.
			
			[(nextEntry _ myHashEntries fetch: (next _ current + 1 \\ tSize)) ~~ NULL
			and: [((nextValue _ myHashValues uIntAt: next) \\ tSize) ~= next]]
				whileTrue: [
					myHashEntries at: current store: nextEntry.
					myHashValues at: current storeUInt: nextValue.
					current _ next.
				].
				
			myHashEntries at: current store: NULL.
			myHashValues at: current storeUInt: UInt32Zero.!
		*/
	}

	/**
	 * Starting at the new item's preferred location and iterating (not recurring!!) around the
	 * set's storage while the slots we're examining are occupied.  (Caller assures us there IS a
	 * vacant slot) if the current occupant is no closer to it's preferred location, exchange it
	 * with the 'new' one.  Bail out if the current occupant IS the new one.
	 * Store the currently 'new' item.
	 */
	public void hashStore(Heaper item, Int32Array values, PtrArray entries) {
		int tSize;
		int current;
		int itemValue;
		int movingValue;
		Heaper movingEntry;
		int movingEntrysHome;
		int sittingValue;
		Heaper sittingEntry;
		int sittingEntrysHome;
		tSize = values.count();
		movingEntry = item;
		movingValue = itemValue = movingEntry.hashForEqual();
		movingEntrysHome = current = movingValue % tSize;
		while ((sittingEntry = entries.fetch(current)) != null) {
			sittingValue = values.int32At(current);
			sittingEntrysHome = sittingValue % tSize;
			/* If the test below is >, new items are stored as far as possible from their desired location, giving the better slots to previous entries.  If it is >=, new items are stored as close as possible to their desired locations, with older items moved farther down, giving the better slots to the more recent items.
						 
						 (Changing this test to > requires moving the duplicate item test.) */
			if (distanceFromHome(current, movingEntrysHome, tSize) >= (distanceFromHome(current, sittingEntrysHome, tSize))) {
				/* Bump the old occupant to another slot. */
				entries.store(current, movingEntry);
				values.storeInt32(current, movingValue);
				movingEntry = sittingEntry;
				movingValue = sittingValue;
				movingEntrysHome = sittingEntrysHome;
				/* If we just picked up the same thing we were originally trying to add,
									 we were trying to insert a duplicate.  We may have reordered the
									 collision set, or we may have just swapped the item with itself, but
									 either way we're done.  (Perhaps we should return an indication that
									 the duplicate was found????) */
				if ((movingValue == itemValue) && (movingEntry.isEqual(item))) {
					return;
				}
				/* item already in set, return. */;
			}
			current = current + 1 % tSize;
		}
		entries.store(current, movingEntry);
		/* Empty slot found.  Drop the new entry into it. */
		values.storeInt32(current, movingValue);
		/*
		udanax-top.st:46641:ActualHashSet methodsFor: 'private: hash resolution'!
		{void} hashStore: item {Heaper} with: values {UInt32Array} with: entries {PtrArray}
			"Starting at the new item's preferred location and iterating (not recurring!!) around the set's storage while the slots we're examining are occupied.  (Caller assures us there IS a vacant slot) if the current occupant is no closer to it's preferred location, exchange it with the 'new' one.  Bail out if the current occupant IS the new one.
			 Store the currently 'new' item."
			| tSize {UInt32} current {UInt32} itemValue {UInt32} 
			  movingValue {UInt32} movingEntry {Heaper} movingEntrysHome {UInt32}
			  sittingValue {UInt32} sittingEntry {Heaper} sittingEntrysHome {UInt32} |
			
			tSize _ values count.
			movingEntry _ item.
			movingValue _ itemValue _ movingEntry hashForEqual.
			movingEntrysHome _ current _ movingValue \\ tSize.
			
			[(sittingEntry _ entries fetch: current) ~~ NULL]
				whileTrue: [
					sittingValue _ values uIntAt: current.
					
					sittingEntrysHome _ sittingValue \\ tSize.
					
					"If the test below is >, new items are stored as far as possible from their desired location, giving the better slots to previous entries.  If it is >=, new items are stored as close as possible to their desired locations, with older items moved farther down, giving the better slots to the more recent items.
					 
					 (Changing this test to > requires moving the duplicate item test.)"
					 
					(self distanceFromHome: current with: movingEntrysHome with: tSize) >=
					(self distanceFromHome: current with: sittingEntrysHome with: tSize)
						ifTrue: [								"Bump the old occupant to another slot."
							entries at: current store: movingEntry.
							values at: current storeUInt: movingValue.
							movingEntry _ sittingEntry.
							movingValue _ sittingValue.
							movingEntrysHome _ sittingEntrysHome.
							
							"If we just picked up the same thing we were originally trying to add,
							 we were trying to insert a duplicate.  We may have reordered the
							 collision set, or we may have just swapped the item with itself, but
							 either way we're done.  (Perhaps we should return an indication that
							 the duplicate was found????)"
							
							((movingValue = itemValue) and: [movingEntry isEqual: item])
								ifTrue: [^ VOID ].				"item already in set, return."
						].
								
					current _ current + 1 \\ tSize.
				].
				
			entries at: current store: movingEntry.				"Empty slot found.  Drop the new entry into it."
			values at: current storeUInt: movingValue.!
		*/
	}

	public int entryTableSize() {
		return myHashEntries.count();
		/*
		udanax-top.st:46693:ActualHashSet methodsFor: 'private: testing access'!
		{UInt32} entryTableSize
			^ myHashEntries count!
		*/
	}

	/**
	 * This method is for regression testing.
	 */
	public void printInternals(PrintWriter oo) {
		int tValue;
		int tSize = myHashValues.count();
		oo.print("tally == ");
		oo.println(myTally);
		for (int idx = 0; idx < myHashEntries.count(); idx++) {
			oo.print(idx);
			oo.print(":	(");
			oo.print(((tValue = myHashValues.int32At(idx)) % tSize));
			oo.print(", ");
			oo.print((distanceFromHome(idx, tValue, tSize)));
			oo.print(") ");
			oo.print(Integer.toHexString(tValue));
			//			/* >>> smalltalkOnly */
			//			tValue.printOnBase(oo, 16);
			//			/* <<< smalltalkOnly */
			/* translateOnly "{
						char	buffer[9];
						sprintf(buffer, \"%X\", tValue);
						oo << buffer;
					}" */
			oo.print(",	");
			oo.println((myHashEntries.fetch(idx)));
		}
		oo.println();
		/*
		udanax-top.st:46697:ActualHashSet methodsFor: 'private: testing access'!
		{void} printInternals: oo {ostream reference}
			"This method is for regression testing."
			
			| tSize {UInt32} tValue {UInt32} |
			
			tSize _ myHashValues count.
			oo << 'tally == ' << myTally << '
		'.
			UInt32Zero almostTo: myHashEntries count do: [:idx {UInt32} |
				oo << idx << ':	(' << ((tValue _ myHashValues uIntAt: idx) \\ tSize).
				oo << ', ' << (self distanceFromHome: idx with: tValue with: tSize) << ') '.
				[ tValue printOn: oo base: 16.] smalltalkOnly.
				'{
					char	buffer[9];
					sprintf(buffer, "%X", tValue);
					oo << buffer;
				}' translateOnly.
				oo << ',	' << (myHashEntries fetch: idx) << '
		'].
			oo << '
		'!
		*/
	}

	/**
	 * Make myHashEntries large enough that we won't grow.
	 */
	public void receiveHashSet(Rcvr rcvr) {
		int count;
		count = LPPrimeSizeProvider.make().uInt32PrimeAfter((myTally * 2));
		myHashEntries = (SharedPtrArray) SharedPtrArray.make(count);
		myHashEntries.shareMore();
		myHashValues = Int32Array.make(count);
		for (int i = 0; i < myTally; i++) {
			hashStore(rcvr.receiveHeaper(), myHashValues, myHashEntries);
		}
		/*
		udanax-top.st:46722:ActualHashSet methodsFor: 'hooks:'!
		{void RECEIVE.HOOK} receiveHashSet: rcvr {Rcvr} 
			"Make myHashEntries large enough that we won't grow."
			
			| count {Int32} |
			count _ LPPrimeSizeProvider make uInt32PrimeAfter: (myTally * 2).
			myHashEntries _ SharedPtrArray make: count.
			myHashEntries shareMore.
			myHashValues _ UInt32Array make: count.
			myTally timesRepeat: [self hashStore: rcvr receiveHeaper with: myHashValues with: myHashEntries]!
		*/
	}

	/**
	 * This currently doesn't take advantage of the optimizations in TableEntries.  It should.
	 */
	public void sendHashSet(Xmtr xmtr) {
		int count = 0;
		Stepper stepper = stepper();
		try {
			Heaper value;
			while ((value = (Heaper) stepper.fetch()) != null) {
				xmtr.sendHeaper(value);
				count = count + 1;
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		if (count == myTally) {
			throw new IllegalStateException("Must write every element");
		}
		/*
		udanax-top.st:46732:ActualHashSet methodsFor: 'hooks:'!
		{void SEND.HOOK} sendHashSet: xmtr {Xmtr}
			"This currently doesn't take advantage of the optimizations in TableEntries.  It should."
			
			| count {Int32} |
			count _ Int32Zero.
			self stepper forEach: [:value {Heaper} |
				xmtr sendHeaper: value.
				count _ count + 1].
			count == myTally assert: 'Must write every element'.!
		*/
	}

	public ActualHashSet(Rcvr receiver) {
		super(receiver);
		myTally = receiver.receiveInt32();
		receiveHashSet(receiver);
		/*
		udanax-top.st:46744:ActualHashSet methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myTally _ receiver receiveInt32.
			self receiveHashSet: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendInt32(myTally);
		sendHashSet(xmtr);
		/*
		udanax-top.st:46749:ActualHashSet methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendInt32: myTally.
			self sendHashSet: xmtr.!
		*/
	}

	//	public static void cleanupGarbage() {
	//		/* >>> smalltalkOnly */
	//		AddTallys = null;
	//		DeleteTallys = null;
	//		TestTallys = null;
	//		StepperTally = null;
	//		/* <<< smalltalkOnly */
	//		/*
	//		udanax-top.st:46766:ActualHashSet class methodsFor: 'smalltalk: initialization'!
	//		cleanupGarbage
	//			[AddTallys _ nil.
	//			DeleteTallys _ nil.
	//			TestTallys _ nil.
	//			StepperTally _ nil] smalltalkOnly.!
	//		*/
	//	}

	//	public static void initTimeNonInherited() {
	//		REQUIRES(LPPrimeSizeProvider.getCategory());
	//		/* ((5 - 10) + 17) == ((5 - 10) \\ 17) assert: 
	//				'incorrect modulus - change HashSet distanceFromHome' */;
	//		/*
	//		udanax-top.st:46772:ActualHashSet class methodsFor: 'smalltalk: initialization'!
	//		initTimeNonInherited
	//			self REQUIRES: LPPrimeSizeProvider.
	//			"((5 - 10) + 17) == ((5 - 10) \\ 17) assert: 
	//				'incorrect modulus - change HashSet distanceFromHome'"!
	//		*/
	//	}

	//	public static void linkTimeNonInherited() {
	//		/* >>> smalltalkOnly */
	//		AddTallys = Array.newWithAll(500, 0);
	//		DeleteTallys = Array.newWithAll(500, 0);
	//		TestTallys = Array.newWithAll(500, 0);
	//		StepperTally = Array.newWithAll(500, 0);
	//		AddOver = DeleteOver = TestOver = StepperOver = 0;
	//		StepperCount = NewSetCount = SetKillCount = 0;
	//		/* <<< smalltalkOnly */
	//		/*
	//		udanax-top.st:46777:ActualHashSet class methodsFor: 'smalltalk: initialization'!
	//		linkTimeNonInherited
	//			[AddTallys _ Array new: 500 withAll: 0.
	//			DeleteTallys _ Array new: 500 withAll: 0.
	//			TestTallys _ Array new: 500 withAll: 0.
	//			StepperTally _ Array new: 500 withAll: 0.
	//			AddOver _ DeleteOver _ TestOver _ StepperOver _ 0.
	//			StepperCount _ NewSetCount _ SetKillCount _ 0.] smalltalkOnly!
	//		*/
	//	}

	public static MuSet make() {
		return new ActualHashSet(0, (SharedPtrArray)(SharedPtrArray.make(7)));
		/*
		udanax-top.st:46788:ActualHashSet class methodsFor: 'pseudo constructors'!
		make
			^ActualHashSet create: Int32Zero with: (SharedPtrArray make: 7)!
		*/
	}

	public static MuSet make(Heaper something) {
		MuSet set = ActualHashSet.make(IntegerValue.one());
		set.store(something);
		return set;
		/*
		udanax-top.st:46791:ActualHashSet class methodsFor: 'pseudo constructors'!
		make.Heaper: something {Heaper}
			
			| set {ActualHashSet} |
			set _ ActualHashSet make.IntegerVar: 1.
			set store: something.
			^ set!
		*/
	}

	public static MuSet make(IntegerValue someSize) {
		return new ActualHashSet(0, (SharedPtrArray)(SharedPtrArray.make((LPPrimeSizeProvider.make().uInt32PrimeAfter(someSize.asInt32())))));
		/*
		udanax-top.st:46798:ActualHashSet class methodsFor: 'pseudo constructors'!
		make.IntegerVar: someSize {IntegerVar}
			^ActualHashSet create: Int32Zero with: (SharedPtrArray make: (LPPrimeSizeProvider make uInt32PrimeAfter: someSize DOTasLong))!
		*/
	}

	//public static String arrayStats(Array array) {
	//Object oo;
	//Object minIdx;
	//Object maxIdx;
	//Object idx;
	//Object medCnt;
	//Object mode;
	//Object modeVal;
	//Object totCnt;
	//Object avg;
	//oo = "".asText().writeStream();
	//minIdx = 0;
	//maxIdx = 0;
	//idx = 1;
	//while (minIdx == 0 && (idx < array.size())) {
	//if (array.at(idx) > 0) {
	//minIdx = idx;
	//}
	//else {
	//idx = idx + 1;
	//}
	//}
	//idx = array.size();
	//while (maxIdx == 0 && (idx > 0)) {
	//if (array.at(idx) > 0) {
	//maxIdx = idx;
	//}
	//else {
	//idx = idx - 1;
	//}
	//}
	//medCnt = 0;
	//mode = 0;
	//modeVal = 0;
	//for (int i = minIdx; i <= maxIdx; i += 1) {
	//Object cv;
	//cv = array.at(i);
	//medCnt = medCnt + (cv * i);
	//totCnt = totCnt + cv;
	//if (cv > modeVal) {
	//mode = i;
	//modeVal = cv;
	//}
	//}
	//avg = totCnt / (maxIdx - minIdx);
	///*
	//udanax-top.st:46803:ActualHashSet class methodsFor: 'smalltalk: instrumentation'!
	//{String} arrayStats: array {Array}
	//	| oo minIdx maxIdx idx medCnt mode modeVal totCnt avg |
	//	oo _ '' asText writeStream.
	//	minIdx _ 0.
	//	maxIdx _ 0.
	//	idx _ 1.
	//	[minIdx = 0 and: [idx < array size]] whileTrue: [
	//		(array at: idx) > 0 ifTrue: [minIdx _ idx]
	//		ifFalse: [idx _ idx + 1]].
	//	idx _ array size.
	//	[maxIdx = 0 and: [idx > 0]] whileTrue: [
	//		(array at: idx) > 0 ifTrue: [maxIdx _ idx]
	//			ifFalse: [idx _ idx - 1]].
	//	medCnt _ 0.
	//	mode _ 0. modeVal _ 0.
	//	minIdx to: maxIdx do: [:i | | cv |
	//		cv _ array at: i.
	//		medCnt _ medCnt + (cv * i).
	//		totCnt _ totCnt + cv.
	//		cv > modeVal ifTrue: [mode _ i. modeVal _ cv]].
	//	avg _ totCnt / (maxIdx - minIdx).!
	//*/
	//}

	//public static void countAdd(IntegerVar tally) {
	//if (tally < AddTallys.size()) {
	//AddTallys.atPut(tally + 1, ((AddTallys.at(tally + 1)) + 1));
	//}
	//else {
	//AddOver = AddOver + 1;
	//}
	///*
	//udanax-top.st:46825:ActualHashSet class methodsFor: 'smalltalk: instrumentation'!
	//{void} countAdd: tally {IntegerVar}
	//	tally < AddTallys size
	//		ifTrue: [AddTallys at: tally+1 put: ((AddTallys at: tally+1) + 1)]
	//		ifFalse: [AddOver _ AddOver + 1]!
	//*/
	//}

	//public static void countDelete(IntegerVar tally) {
	//if (tally < DeleteTallys.size()) {
	//DeleteTallys.atPut(tally + 1, ((DeleteTallys.at(tally + 1)) + 1));
	//}
	//else {
	//DeleteOver = DeleteOver + 1;
	//}
	///*
	//udanax-top.st:46830:ActualHashSet class methodsFor: 'smalltalk: instrumentation'!
	//{void} countDelete: tally {IntegerVar}
	//	tally < DeleteTallys size
	//		ifTrue: [DeleteTallys at: tally+1 put: ((DeleteTallys at: tally+1) + 1)]
	//		ifFalse: [DeleteOver _ DeleteOver + 1]!
	//*/
	//}

	//public static void countStepper(IntegerVar tally) {
	//if (tally < StepperTally.size()) {
	//StepperTally.atPut(tally + 1, ((StepperTally.at(tally + 1)) + 1));
	//}
	//else {
	//StepperOver = StepperOver + 1;
	//}
	///*
	//udanax-top.st:46836:ActualHashSet class methodsFor: 'smalltalk: instrumentation'!
	//{void} countStepper: tally {IntegerVar}
	//	tally < StepperTally size
	//		ifTrue: [StepperTally at: tally+1 put: ((StepperTally at: tally+1) + 1)]
	//		ifFalse: [StepperOver _ StepperOver + 1]!
	//*/
	//}

	//public static void countTest(IntegerVar tally) {
	//if (tally < TestTallys.size()) {
	//TestTallys.atPut(tally + 1, ((TestTallys.at(tally + 1)) + 1));
	//}
	//else {
	//TestOver = TestOver + 1;
	//}
	///*
	//udanax-top.st:46841:ActualHashSet class methodsFor: 'smalltalk: instrumentation'!
	//{void} countTest: tally {IntegerVar}
	//	tally < TestTallys size
	//		ifTrue: [TestTallys at: tally+1 put: ((TestTallys at: tally+1) + 1)]
	//		ifFalse: [TestOver _ TestOver + 1]!
	//*/
	//}
}
