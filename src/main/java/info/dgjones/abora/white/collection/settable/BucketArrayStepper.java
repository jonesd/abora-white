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
package info.dgjones.abora.white.collection.settable;

import info.dgjones.abora.white.cache.InstanceCache;
import info.dgjones.abora.white.collection.arrays.SharedPtrArray;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.collection.steppers.TableStepper;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class BucketArrayStepper extends TableStepper {
	protected TableEntry myEntry;
	protected SharedPtrArray myEntries;
	protected int myNextBucket;
	protected static InstanceCache SomeSteppers = InstanceCache.make(8);
	
	/*
	udanax-top.st:55515:
	TableStepper subclass: #BucketArrayStepper
		instanceVariableNames: '
			myEntry {TableEntry | NULL}
			myEntries {SharedPtrArray of: TableEntry}
			myNextBucket {Int4}'
		classVariableNames: 'SomeSteppers {InstanceCache} '
		poolDictionaries: ''
		category: 'Xanadu-Collection-SetTable'!
	*/
	/*
	udanax-top.st:55522:
	(BucketArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:55594:
	BucketArrayStepper class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:55597:
	(BucketArrayStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public Heaper fetch() {
		if (myEntry == null) {
			return null;
		} else {
			return myEntry.value();
		}
		/*
		udanax-top.st:55527:BucketArrayStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			myEntry == NULL ifTrue: [^NULL] ifFalse: [^myEntry value]!
		*/
	}

	public boolean hasValue() {
		return myEntry != null;
		/*
		udanax-top.st:55530:BucketArrayStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^myEntry ~~ NULL!
		*/
	}

	public void step() {
		if (myEntry != null) {
			myEntry = myEntry.fetchNext();
			verifyEntry();
		}
		/*
		udanax-top.st:55533:BucketArrayStepper methodsFor: 'operations'!
		{void} step
			myEntry ~~ NULL ifTrue: [
				myEntry _ myEntry fetchNext.
				self verifyEntry]!
		*/
	}

	/**
	 * Step through the bucket till we find something with a matching key.
	 */
	public void verifyEntry() {
		int bucket;
		/* use a local index to avoid pointer refs in loop */
		if (myEntry != null) {
			return;
		}
		bucket = myNextBucket;
		while (bucket < myEntries.count()) {
			Heaper nextEntry;
			nextEntry = myEntries.fetch(bucket);
			bucket = bucket + 1;
			if (nextEntry != null) {
				myEntry = (TableEntry) nextEntry;
				myNextBucket = bucket;
				return;
			}
		}
		myEntries = null;
		/*
		udanax-top.st:55540:BucketArrayStepper methodsFor: 'private:'!
		{void} verifyEntry
			"Step through the bucket till we find something with a matching key."
			| bucket {Int32} |
			"use a local index to avoid pointer refs in loop"
			myEntry ~~ NULL ifTrue: [^VOID].
			bucket _ myNextBucket.
			[bucket < myEntries count] whileTrue:
				[|nextEntry {Heaper wimpy}|
				nextEntry _ myEntries fetch: bucket.
				bucket _ bucket + 1.
				nextEntry ~~ NULL ifTrue:
					[myEntry _ nextEntry cast: TableEntry.
					myNextBucket _ bucket.
					^VOID]].
			myEntries := NULL.!
		*/
	}

	public IntegerValue index() {
		if (myEntry != null) {
			throw new IllegalStateException("Illegal access");
		};
		return myEntry.index();
		/*
		udanax-top.st:55558:BucketArrayStepper methodsFor: 'special'!
		{IntegerVar} index
			myEntry ~~ NULL assert: 'Illegal access'.
			^myEntry index!
		*/
	}

	public Position position() {
		if (myEntry != null) {
			throw new IllegalStateException("Illegal access");
		}
		return myEntry.position();
		/*
		udanax-top.st:55562:BucketArrayStepper methodsFor: 'special'!
		{Position} position
			myEntry ~~ NULL assert: 'Illegal access'.
			^myEntry position!
		*/
	}

	public BucketArrayStepper(SharedPtrArray entries, TableEntry entry, int nextBucket) {
		super();
		myEntry = entry;
		myEntries = entries;
		myNextBucket = nextBucket;
		myEntries.shareMore();
		verifyEntry();
		/*
		udanax-top.st:55568:BucketArrayStepper methodsFor: 'protected: create'!
		create: entries {SharedPtrArray} with: entry {TableEntry | NULL} with: nextBucket {Int32}
			super create.
			myEntry _ entry.
			myEntries _ entries.
			myNextBucket _ nextBucket.
			myEntries shareMore.
			self verifyEntry!
		*/
	}

	public void destruct() {
		if (myEntries != null) {
			myEntries.shareLess();
		}
		super.destruct();
		/*
		udanax-top.st:55576:BucketArrayStepper methodsFor: 'protected: create'!
		{void} destruct
			myEntries ~~ NULL ifTrue: [
				myEntries shareLess].
			super destruct!
		*/
	}

	public Stepper copy() {
		Heaper result;
		result = SomeSteppers.fetch();
		if (result == null) {
			return new BucketArrayStepper(myEntries, myEntry, myNextBucket);
		} else {
			//TODO review translation
			return new BucketArrayStepper(myEntries, myEntry, myNextBucket);
		}
		/*
		udanax-top.st:55583:BucketArrayStepper methodsFor: 'create'!
		{Stepper} copy
			| result {Heaper} |
			result := SomeSteppers fetch.
			result ==  NULL
				ifTrue: [^BucketArrayStepper create: myEntries with: myEntry with: myNextBucket]
				ifFalse: [^(BucketArrayStepper new.Become: result) create: myEntries with: myEntry with: myNextBucket]!
		*/
	}

	public void destroy() {
		if (!(SomeSteppers.store(this))) {
			super.destroy();
		}
		/*
		udanax-top.st:55590:BucketArrayStepper methodsFor: 'create'!
		{void} destroy
			(SomeSteppers store: self) ifFalse: [super destroy]!
		*/
	}

//	public static void initTimeNonInherited() {
//		SomeSteppers = InstanceCache.make(8);
//		/*
//		udanax-top.st:55602:BucketArrayStepper class methodsFor: 'smalltalk: init'!
//		initTimeNonInherited
//			SomeSteppers := InstanceCache make: 8!
//		*/
//	}
//
//	public static void linkTimeNonInherited() {
//		SomeSteppers = null;
//		/*
//		udanax-top.st:55605:BucketArrayStepper class methodsFor: 'smalltalk: init'!
//		linkTimeNonInherited
//			SomeSteppers := NULL!
//		*/
//	}

	public static TableStepper make(SharedPtrArray entries) {
		Heaper result;
		result = SomeSteppers.fetch();
		if (result == null) {
			return new BucketArrayStepper(entries, null, 0);
		} else {
			//TODO review translation
			return new BucketArrayStepper(entries, null, 0);
		}
		/*
		udanax-top.st:55610:BucketArrayStepper class methodsFor: 'creation'!
		{TableStepper} make: entries {SharedPtrArray}
			| result {Heaper} |
			result := SomeSteppers fetch.
			result == NULL
				ifTrue: [^self create: entries with: NULL with: Int32Zero]
				ifFalse: [^(self new.Become: result) create: entries with: NULL with: Int32Zero]!
		*/
	}
}
