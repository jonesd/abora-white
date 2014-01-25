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
package info.dgjones.abora.white.collection.tables;

import info.dgjones.abora.white.collection.steppers.ArrayAccumulator;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.collection.steppers.TableAccumulator;
import info.dgjones.abora.white.collection.steppers.TableStepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.spaces.integers.IntegerSpace;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * The class XuArray is intended to model zero-based arrays with integer keys (indices).
 * This makes them like the array primitive in C and C++.  There is an additional constraint,
 * which is they are to have simple domains.  Therefore they should not be constructed with
 * non-contiguous sections.  This is not currently enforced.  Given that it is enforced, an
 * XuArray with count N would have as its domain exactly the integers from 0 to N-1.
 * <p>
 * There is some controversy over whether XuArray should be a type and enforce this contraint
 * (by BLASTing if an attempt is made to violate the constraint), or whether XuArray is just
 * a specialized implementation for when an IntegerTable happens to meet this constraint; in
 * which case it should "become" a more general implementation when an attempt is made to
 * violate the constraint (see "Type Safe Become").  In the latter case, XuArray will
 * probably be made a private class as well.  Please give us your opinion.
 * XuArray provides no additional protocol.
 */
public abstract class MuArray extends IntegerTable {
	//	protected static Signal MustBeContiguousDomainSignal;
	/*
	udanax-top.st:49108:
	IntegerTable subclass: #MuArray
		instanceVariableNames: ''
		classVariableNames: 'MustBeContiguousDomainSignal {Signal smalltalk} '
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:49112:
	MuArray comment:
	'The class XuArray is intended to model zero-based arrays with integer keys (indices).
		
		This makes them like the array primitive in C and C++.  There is an additional constraint, which is they are to have simple domains.  Therefore they should not be constructed with non-contiguous sections.  This is not currently enforced.  Given that it is enforced, an XuArray with count N would have as its domain exactly the integers from 0 to N-1.
		
		There is some controversy over whether XuArray should be a type and enforce this contraint (by BLASTing if an attempt is made to violate the constraint), or whether XuArray is just a specialized implementation for when an IntegerTable happens to meet this constraint; in which case it should "become" a more general implementation when an attempt is made to violate the constraint (see "Type Safe Become").  In the latter case, XuArray will probably be made a private class as well.  Please give us your opinion.
		
		XuArray provides no additional protocol.'!
	*/
	/*
	udanax-top.st:49120:
	(MuArray getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:49243:
	MuArray class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:49246:
	(MuArray getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected MuArray() {
		super();
	}

	protected MuArray(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	/**
	 * A new empty XnArray
	 */
	public static MuArray array() {
		return (MuArray) MuArray.make(IntegerValue.one());
		/*
		udanax-top.st:49251:MuArray class methodsFor: 'creation'!
		{MuArray INLINE} array
			"A new empty XnArray"
			
			^MuArray make.IntegerVar: 1!
		*/
	}

	/**
	 * A new XnArray initialized with a single element, 'obj0', stored at index 0.
	 */
	public static MuArray array(Heaper obj0) {
		MuArray table = (MuArray) MuArray.make(IntegerValue.one());
		table.atIntStore(IntegerValue.zero(), obj0);
		return table;
		/*
		udanax-top.st:49256:MuArray class methodsFor: 'creation'!
		{MuArray} array: obj0 {Heaper}
			"A new XnArray initialized with a single element, 'obj0', stored at index 0."
			
			| table {MuArray} |
			table _ MuArray make.IntegerVar: 1.
			table atInt: IntegerVar0 store: obj0.
			^table!
		*/
	}

	/**
	 * A new XnArray initialized with a two elements stored at indicies 0 and 1.
	 */
	public static MuArray array(Heaper obj0, Heaper obj1) {
		MuArray table = (MuArray) MuArray.make(IntegerValue.make(2));
		table.atIntStore(IntegerValue.zero(), obj0);
		table.atIntStore(IntegerValue.one(), obj1);
		return table;
		/*
		udanax-top.st:49264:MuArray class methodsFor: 'creation'!
		{MuArray} array: obj0 {Heaper} with: obj1 {Heaper}
			"A new XnArray initialized with a two elements stored at indicies 0 and 1."
			
			| table {MuArray} |
			table _ MuArray make.IntegerVar: 2.
			table atInt: IntegerVar0 store: obj0.
			table atInt: 1 store: obj1.
			^table!
		*/
	}

	/**
	 * A new XuArray initialized with a three elements stored at indicies 0, 1, and 2.
	 */
	public static MuArray array(Heaper obj0, Heaper obj1, Heaper obj2) {
		MuArray table = (MuArray) MuArray.make(IntegerValue.make(3));
		table.atIntStore(IntegerValue.zero(), obj0);
		table.atIntStore(IntegerValue.one(), obj1);
		table.atIntStore(IntegerValue.make(2), obj2);
		return table;
		/*
		udanax-top.st:49273:MuArray class methodsFor: 'creation'!
		{MuArray} array: obj0 {Heaper} with: obj1 {Heaper} with: obj2 {Heaper}
			"A new XuArray initialized with a three elements stored at indicies 0, 1, and 2."
			
			| table {MuArray} |
			table _ MuArray make.IntegerVar: 3.
			table atInt: IntegerVar0 store: obj0.
			table atInt: 1 store: obj1.
			table atInt: 2 store: obj2.
			^table!
		*/
	}

	/**
	 * A new XuArray initialized with a four elements stored at indicies 0 through 3.
	 */
	public static MuArray array(Heaper obj0, Heaper obj1, Heaper obj2, Heaper obj3) {
		MuArray table = (MuArray) MuArray.make(IntegerValue.make(4));
		table.atIntStore(IntegerValue.zero(), obj0);
		table.atIntStore(IntegerValue.one(), obj1);
		table.atIntStore(IntegerValue.make(2), obj2);
		table.atIntStore(IntegerValue.make(3), obj3);
		return table;
		/*
		udanax-top.st:49283:MuArray class methodsFor: 'creation'!
		{MuArray} array: obj0 {Heaper} 
			with: obj1 {Heaper} 
			with: obj2 {Heaper} 
			with: obj3 {Heaper}
			"A new XuArray initialized with a four elements stored at indicies 0 through 3."
			
			| table {MuArray} |
			table _ MuArray make.IntegerVar: 4.
			table atInt: IntegerVar0 store: obj0.
			table atInt: 1 store: obj1.
			table atInt: 2 store: obj2.
			table atInt: 3 store: obj3.
			^table!
		*/
	}

	/**
	 * Returns an Accumulator which will produce an XuArray of the elements
	 * accumulated into it in order of accumulation. See XuArray. Equivalent to
	 * 'tableAccumulator()'. Eventually either he or I should be declared obsolete.
	 */
	public static TableAccumulator arrayAccumulator() {
		return ArrayAccumulator.make(MuArray.array());
		/*
		udanax-top.st:49297:MuArray class methodsFor: 'creation'!
		{TableAccumulator} arrayAccumulator
			"Returns an Accumulator which will produce an XuArray of the elements 
			accumulated into it in order of accumulation. See XuArray. Equivalent to 
			'tableAccumulator()'. Eventually either he or I should be declared obsolete."
			^ ArrayAccumulator make: MuArray array!
		*/
	}

	/**
	 * An accumulator which will accumulate by appending elements onto the end of
	 * 'onArray'. It is an error for anyone else to modify 'onArray' between creating
	 * this accumulator and accumulating into it. acc->value() will return 'onArray'
	 * itself.
	 */
	public static TableAccumulator arrayAccumulator(MuArray onArray) {
		return ArrayAccumulator.make(onArray);
		/*
		udanax-top.st:49304:MuArray class methodsFor: 'creation'!
		{TableAccumulator} arrayAccumulator: onArray {MuArray} 
			"An accumulator which will accumulate by appending elements onto the end of 
			'onArray'. It is an error for anyone else to modify 'onArray' between creating 
			this accumulator and accumulating into it. acc->value() will return 'onArray' 
			itself."
			^ArrayAccumulator make: onArray!
		*/
	}

	/**
	 * 'someSize' is a hint about how big we should expect the array to need to grow.
	 */
	public static IntegerTable make(IntegerValue someSize) {
		return new ActualArray(someSize);
		/*
		udanax-top.st:49312:MuArray class methodsFor: 'creation'!
		make.IntegerVar: someSize {IntegerVar} 
			"'someSize' is a hint about how big we should expect the array to need to grow."
			^ActualArray create.IntegerVar: someSize!
		*/
	}

	/**
	 * The resulting ScruTable is a view onto 'array'. It is a view in which each key
	 * is offset by 'dsp' from where it is in 'array'. By saying it is a view, we mean
	 * that as 'array' is modified, the view tracks the changes.
	 */
	public static ScruTable offsetScruArray(MuArray array, Dsp dsp) {
		return OffsetScruArray.make(array, dsp);
		/*
		udanax-top.st:49317:MuArray class methodsFor: 'creation'!
		{ScruTable} offsetScruArray: array {MuArray} with: dsp {Dsp} 
			"The resulting ScruTable is a view onto 'array'. It is a view in which each key 
			is offset by 'dsp' from where it is in 'array'. By saying it is a view, we mean 
			that as 'array' is modified, the view tracks the changes."
			^OffsetScruArray make: array with: dsp!
		*/
	}


	/////////////////////////////////////////////
	// Accessing

	public abstract Heaper atIntStore(IntegerValue key, Heaper value);
	/*
	udanax-top.st:49125:MuArray methodsFor: 'accessing'!
	{Heaper} atInt: key {IntegerVar} store: value {Heaper} 
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:49129:MuArray methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		^ IntegerSpace make!
	*/

	public abstract IntegerValue count();
	/*
	udanax-top.st:49133:MuArray methodsFor: 'accessing'!
	{IntegerVar} count
		self subclassResponsibility.!
	*/

	public abstract XnRegion domain();
	/*
	udanax-top.st:49137:MuArray methodsFor: 'accessing'!
	{XnRegion} domain
		self subclassResponsibility.!
	*/

	public abstract IntegerValue highestIndex();
	/*
	udanax-top.st:49141:MuArray methodsFor: 'accessing'!
	{IntegerVar} highestIndex
		self subclassResponsibility!
	*/

	public abstract Heaper intFetch(IntegerValue key);
	/*
	udanax-top.st:49144:MuArray methodsFor: 'accessing'!
	{Heaper} intFetch: key {IntegerVar}
		self subclassResponsibility!
	*/

	public abstract boolean intWipe(IntegerValue anIdx);
	/*
	udanax-top.st:49147:MuArray methodsFor: 'accessing'!
	{BooleanVar} intWipe: anIdx {IntegerVar}
		self subclassResponsibility!
	*/

	public abstract IntegerValue lowestIndex();
	/*
	udanax-top.st:49150:MuArray methodsFor: 'accessing'!
	{IntegerVar} lowestIndex
		self subclassResponsibility!
	*/

	/**
	 * Return a table which contains the elements from start to stop, starting at firstIndex.
	 * Zero-based subclasses will blast if firstIndex is non-zero
	 */
	public ScruTable offsetSubTableBetween(IntegerValue startIndex, IntegerValue stopIndex, IntegerValue firstIndex) {
		return subTableBetween(startIndex, stopIndex);
		/*
		udanax-top.st:49153:MuArray methodsFor: 'accessing'!
		{ScruTable} offsetSubTableBetween: startIndex {IntegerVar} 
			with: stopIndex {IntegerVar} 
			with: firstIndex {IntegerVar unused} 
			"Return a table which contains the elements from start to stop, starting at firstIndex.
			Zero-based subclasses will blast if firstIndex is non-zero"
			^ self subTableBetween: startIndex with: stopIndex!
		*/
	}

	public abstract ScruTable subTable(XnRegion region);
	/*
	udanax-top.st:49161:MuArray methodsFor: 'accessing'!
	{ScruTable} subTable: region {XnRegion} 
		self subclassResponsibility!
	*/

	public abstract ScruTable subTableBetween(IntegerValue startLoc, IntegerValue endLoc);
	/*
	udanax-top.st:49165:MuArray methodsFor: 'accessing'!
	{ScruTable} subTableBetween: startLoc {IntegerVar} with: endLoc {IntegerVar} 
		self subclassResponsibility!
	*/

	public ScruTable transformedBy(Dsp dsp) {
		if (dsp.inverse().isEqual(dsp)) {
			return this;
		} else {
			return MuArray.offsetScruArray(this, dsp);
		}
		/*
		udanax-top.st:49168:MuArray methodsFor: 'accessing'!
		{ScruTable} transformedBy: dsp {Dsp} 
			(dsp inverse isEqual: dsp) 
				ifTrue: [^self]
				ifFalse: [^MuArray offsetScruArray: self with: dsp]!
		*/
	}

	/////////////////////////////////////////////
	// Creation

	public abstract ScruTable copy();
	/*
	udanax-top.st:49176:MuArray methodsFor: 'creation'!
	{ScruTable} copy
		self subclassResponsibility!
	*/

	public abstract ScruTable emptySize(IntegerValue size);
	/*
	udanax-top.st:49179:MuArray methodsFor: 'creation'!
	{ScruTable} emptySize: size {IntegerVar}
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Testing

	public boolean includesIntKey(IntegerValue aKey) {
		return aKey.isGE(IntegerValue.zero()) && (aKey.isLT(count()));
		/*
		udanax-top.st:49185:MuArray methodsFor: 'testing'!
		{BooleanVar} includesIntKey: aKey {IntegerVar}
			^aKey >= IntegerVar0 and: [aKey < self count]!
		*/
	}

	public boolean isEmpty() {
		return count().isEqual(IntegerValue.zero());
		/*
		udanax-top.st:49188:MuArray methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^self count = IntegerVar0!
		*/
	}

	/////////////////////////////////////////////
	// Runs

	public abstract XnRegion runAtInt(IntegerValue key);
	/*
	udanax-top.st:49193:MuArray methodsFor: 'runs'!
	{XnRegion} runAtInt: key {IntegerVar}
		self subclassResponsibility!
	*/

	/////////////////////////////////////////////
	// Enumerating

	/**
	 * Return a stepper on this table.
	 */
	public abstract TableStepper stepper(OrderSpec order);
	/*
	udanax-top.st:49198:MuArray methodsFor: 'enumerating'!
	{TableStepper} stepper: order {OrderSpec default: NULL}
		"Return a stepper on this table."
		self subclassResponsibility!
	*/

	public Heaper theOne() {
		if (!count().equals(IntegerValue.one())) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
		}
		return intFetch(IntegerValue.zero());
		/*
		udanax-top.st:49203:MuArray methodsFor: 'enumerating'!
		{Heaper} theOne
			self count ~~ 1 ifTrue:
				[ Heaper BLAST: #NotOneElement ].
			^ self intFetch: IntegerVar0!
		*/
	}

	/////////////////////////////////////////////
	// Bulk Operations

	/**
	 * I 'wipe' from myself all associations whose key
	 * is in 'region'. See MuTable::wipe
	 */
	public void wipeAll(XnRegion region) {
		if (!(region.coordinateSpace().isEqual(coordinateSpace()))) {
			throw new AboraRuntimeException(AboraRuntimeException.WRONG_COORD_SPACE);
		}
		if (isEmpty()) {
			return;
		}
		if (!region.isSimple()) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE);
		}
		Stepper stepper = ((region.intersect(domain())).stepper((IntegerSpace.make().getDescending())));
		try {
			IntegerPos p;
			while ((p = (IntegerPos) stepper.fetch()) != null) {
				intWipe(p.asIntegerVar());
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		/*
		udanax-top.st:49210:MuArray methodsFor: 'bulk operations'!
		{void} wipeAll: region {XnRegion} 
			"I 'wipe' from myself all associations whose key 
			is in 'region'. See MuTable::wipe"
			(region coordinateSpace isEqual: self coordinateSpace)
				ifFalse: [Heaper BLAST: #WrongCoordSpace].
			self isEmpty ifTrue: [^VOID].
			region isSimple ifFalse: [Heaper BLAST: #NotSimple].
			((region intersect: self domain)
				stepper: (IntegerSpace make getDescending))
				forEach: [:p {IntegerPos} | self intWipe: p asIntegerVar]!
		*/
	}

	public Heaper atStore(Position key, Heaper value) {
		return atIntStore(((IntegerPos) key).asIntegerVar(), value);
		/*
		udanax-top.st:49224:MuArray methodsFor: 'overload junk'!
		{Heaper} at: key {Position} store: value {Heaper} 
			^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
		*/
	}

	public Heaper fetch(Position key) {
		return intFetch((((IntegerPos) key).asIntegerVar()));
		/*
		udanax-top.st:49228:MuArray methodsFor: 'overload junk'!
		{Heaper} fetch: key {Position} 
			^ self intFetch: ((key cast: IntegerPos) asIntegerVar)!
		*/
	}

	public boolean includesKey(Position aKey) {
		return includesIntKey((((IntegerPos) aKey).asIntegerVar()));
		/*
		udanax-top.st:49232:MuArray methodsFor: 'overload junk'!
		{BooleanVar} includesKey: aKey {Position}
			^self includesIntKey: ((aKey cast: IntegerPos) asIntegerVar)!
		*/
	}

	public XnRegion runAt(Position key) {
		return runAtInt((((IntegerPos) key).asIntegerVar()));
		/*
		udanax-top.st:49235:MuArray methodsFor: 'overload junk'!
		{XnRegion} runAt: key {Position} 
			^self runAtInt: ((key quickCast: IntegerPos) asIntegerVar)!
		*/
	}

	public boolean wipe(Position key) {
		return intWipe((((IntegerPos) key).asIntegerVar()));
		/*
		udanax-top.st:49239:MuArray methodsFor: 'overload junk'!
		{BooleanVar} wipe: key {Position}
			^ self intWipe: ((key cast: IntegerPos) asIntegerVar)!
		*/
	}
}
