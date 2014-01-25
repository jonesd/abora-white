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

import java.io.PrintWriter;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.collection.steppers.ITAscendingStepper;
import info.dgjones.abora.white.collection.steppers.IntegerTableStepper;
import info.dgjones.abora.white.collection.steppers.TableStepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.spaces.integers.IntegerRegion;
import info.dgjones.abora.white.spaces.integers.IntegerSpace;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * The IntegerTable class is intended to provide an integer indexed
 * table which is not constrained to be zero based.
 */
public class ActualIntegerTable extends OberIntegerTable {
	protected PtrArray elements;
	protected IntegerValue start;
	protected int elemCount;
	protected int firstElem;
	protected int lastElem;
	protected int tally;
	protected boolean domainIsSimple;
	/*
	udanax-top.st:49748:
	OberIntegerTable subclass: #ActualIntegerTable
		instanceVariableNames: '
			elements {PtrArray}
			start {IntegerVar}
			elemCount {UInt32}
			firstElem {UInt32}
			lastElem {UInt32}
			tally {UInt32}
			domainIsSimple {BooleanVar}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:49759:
	ActualIntegerTable comment:
	'The IntegerTable class is intended to provide an integer indexed
	table which is not constrained to be zero based.'!
	*/
	/*
	udanax-top.st:49762:
	(ActualIntegerTable getOrMakeCxxClassDescription)
		friends:
	'/- friends for class ActualIntegerTable -/
	friend class ITAscendingStepper;
	friend class ITDescendingStepper;
	friend class ITGenericStepper;';
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	public int fastHash() {
		return (((start.asInt32() ^ firstElem) ^ tally) ^ lastElem) + getClass().hashCode();
		/*
		udanax-top.st:49772:ActualIntegerTable methodsFor: 'testing'!
		{UInt32} fastHash
			^(((start DOTasLong bitXor: firstElem) bitXor: tally) bitXor: lastElem) + #cat.U.ActualIntegerTable hashForEqual!
		*/
	}

	public boolean includesIntKey(IntegerValue aKey) {
		if (aKey.isLT(lowestIndex()) || (aKey.isGT(highestIndex()))) {
			return false;
		} else {
			if (domainIsSimple) {
				return true;
			} else {
				return (elements.fetch(aKey.asInt32() - start.asInt32())) != null;
			}
		}
		/*
		udanax-top.st:49775:ActualIntegerTable methodsFor: 'testing'!
		{BooleanVar} includesIntKey: aKey {IntegerVar} 
			(aKey < self lowestIndex or: [aKey > self highestIndex])
				ifTrue: [^false]
				ifFalse: [domainIsSimple
						ifTrue: [^true]
						ifFalse: [^(elements fetch: aKey DOTasLong - start DOTasLong) ~~ NULL]]!
		*/
	}

	public boolean isEmpty() {
		return tally == 0;
		/*
		udanax-top.st:49782:ActualIntegerTable methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^tally == UInt32Zero!
		*/
	}

	public Heaper atIntStore(IntegerValue index, Heaper value) {
		Heaper old;
		if (value == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NULL_INSERTION);
		}
		if (tally == 0) {
			start = index;
		}
		if (index.minus(start).isGE(IntegerValue.make(elemCount))) {
			enlargeAfter(index);
		} else {
			if (index.isLT(start)) {
				enlargeBefore(index);
			}
		}
		int reali = (index.minus(start)).asInt32();
		if ((old = elements.fetch(reali)) == null) {
			tally = tally + 1;
		}
		if (reali < firstElem) {
			if ((firstElem - reali) > 1) {
				domainIsSimple = false;
			}
			firstElem = reali;
		}
		if (reali > lastElem) {
			if ((reali - lastElem) > 1) {
				domainIsSimple = false;
			}
			lastElem = reali;
		}
		elements.store(reali, value);
		return old;
		/*
		udanax-top.st:49787:ActualIntegerTable methodsFor: 'accessing'!
		{Heaper} atInt: index {IntegerVar} store: value {Heaper} 
			| reali {Int32} old {Heaper} |
			value == NULL ifTrue: [Heaper BLAST: #NullInsertion].
			tally == UInt32Zero ifTrue: [start _ index].
			index - start >= elemCount
				ifTrue: [self enlargeAfter: index]
				ifFalse: [index < start ifTrue: [self enlargeBefore: index]].
			reali _ (index - start) DOTasLong.
			(old _ elements fetch: reali) == NULL ifTrue: [tally _ tally + 1].
			reali < firstElem ifTrue: [
				(firstElem - reali) > 1 ifTrue: [domainIsSimple _ false].
				firstElem _ reali].
			reali > lastElem ifTrue: [
				(reali - lastElem) > 1 ifTrue: [domainIsSimple _ false].
				lastElem _ reali].
			elements at: reali store: value.
			^ old!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:49805:ActualIntegerTable methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^IntegerSpace make!
		*/
	}

	public IntegerValue count() {
		return IntegerValue.make(tally);
		/*
		udanax-top.st:49809:ActualIntegerTable methodsFor: 'accessing'!
		{IntegerVar} count
			^tally!
		*/
	}

	/**
	 */
	public XnRegion domain() {
		/* The domainIsSimple flag is used as an optimization in this method.  When it is True, I 
			stop looking after the first simple domain I find.  Therefore, when True, it MUST BE 
			CORRECT.  When it is False, I do a complete search, and set the flag if the domain
			turns out to be simple. */
		XnRegion newReg;
		if (isEmpty()) {
			return IntegerRegion.make();
		} else {
			if (domainIsSimple) {
				return IntegerRegion.make(lowestIndex(), highestIndex().plus(IntegerValue.one()));
			} else {
				newReg = generateDomain();
				if (newReg.isSimple()) {
					domainIsSimple = true;
				}
				return newReg;
			}
		}
		/*
		udanax-top.st:49813:ActualIntegerTable methodsFor: 'accessing'!
		{XnRegion} domain
			""
			"The domainIsSimple flag is used as an optimization in this method.  When it is True, I 
			stop looking after the first simple domain I find.  Therefore, when True, it MUST BE 
			CORRECT.  When it is False, I do a complete search, and set the flag if the domain
			turns out to be simple."
			| newReg {XnRegion} |
			self isEmpty
				ifTrue: [^IntegerRegion make]
				ifFalse: [domainIsSimple
						ifTrue: 
							[^IntegerRegion make: self lowestIndex with: self highestIndex + 1]
						ifFalse: 
							[newReg _ self generateDomain.
							newReg isSimple ifTrue: [domainIsSimple _ true].
							^newReg]]!
		*/
	}

	public IntegerValue highestIndex() {
		if (tally == 0) {
			return start;
		}
		return start.plus(IntegerValue.make(lastElem));
		/*
		udanax-top.st:49831:ActualIntegerTable methodsFor: 'accessing'!
		{IntegerVar} highestIndex
			tally == UInt32Zero ifTrue: [^start].
			^ start + lastElem!
		*/
	}

	public Heaper intFetch(IntegerValue index) {
		int idx;
		if ((idx = (index.minus(start)).asInt32()) >= elemCount || (index.isLT(start))) {
			return null;
		} else {
			return elements.fetch(idx);
		}
		/*
		udanax-top.st:49835:ActualIntegerTable methodsFor: 'accessing'!
		{Heaper} intFetch: index {IntegerVar} 
			| idx {UInt32}|
			((idx _ (index-start) DOTasLong) >= elemCount or: [index < start])
				ifTrue: [^NULL]
				ifFalse: [^elements fetch: idx]!
		*/
	}

	public boolean intWipe(IntegerValue index) {
		int reali;
		boolean wiped;
		wiped = false;
		reali = (index.minus(start)).asInt32();
		if (!(reali > lastElem || (reali < firstElem))) {
			if ((elements.fetch(reali)) != null) {
				tally = tally - 1;
				wiped = true;
			}
			elements.store(reali, null);
			if (reali == firstElem) {
				firstElem = firstElemAfter(reali);
			} else {
				if (reali == lastElem) {
					lastElem = lastElemBefore(reali);
				} else {
					domainIsSimple = false;
				}
			}
		}
		return wiped;
		/*
		udanax-top.st:49842:ActualIntegerTable methodsFor: 'accessing'!
		{BooleanVar} intWipe: index {IntegerVar} 
			| reali {UInt32} wiped {BooleanVar} |
			wiped _ false.
			reali _ (index - start) DOTasLong.
			(reali > lastElem or: [reali < firstElem])
				ifFalse: 
					[(elements fetch: reali) ~~ NULL ifTrue: [tally _ tally - 1. wiped _ true].
					elements at: reali store: NULL.
					reali == firstElem
						ifTrue: [firstElem _ self firstElemAfter: reali]
						ifFalse: [reali == lastElem
								ifTrue: [lastElem _ self lastElemBefore: reali]
								ifFalse: [domainIsSimple _ false]]].
			^ wiped!
		*/
	}

	public IntegerValue lowestIndex() {
		if (tally == 0) {
			return start;
		}
		return start.plus(IntegerValue.make(firstElem));
		/*
		udanax-top.st:49857:ActualIntegerTable methodsFor: 'accessing'!
		{IntegerVar} lowestIndex
			tally == UInt32Zero ifTrue: [^start].
			^ start + firstElem!
		*/
	}

	public ScruTable copy() {
		return new ActualIntegerTable(((PtrArray) elements.copy()), start, elemCount, firstElem, lastElem, tally, domainIsSimple);
		/*
		udanax-top.st:49864:ActualIntegerTable methodsFor: 'creation'!
		{ScruTable} copy
			^ ActualIntegerTable 
				create: (elements copy cast: PtrArray)
				with: start
				with: elemCount
				with: firstElem
				with: lastElem
				with: tally
				with: domainIsSimple!
		*/
	}

	/**
	 * The optional argument just hints at the number of elements
	 * to eventually be added.  It makes no difference semantically.
	 */
	public ActualIntegerTable() {
		super();
		elements = PtrArray.make(8);
		start = IntegerValue.zero();
		firstElem = 7;
		lastElem = 0;
		elemCount = 8;
		tally = 0;
		domainIsSimple = true;
		/*
		udanax-top.st:49874:ActualIntegerTable methodsFor: 'creation'!
		create
			"The optional argument just hints at the number of elements
			 to eventually be added.  It makes no difference semantically."
			super create.
			elements _ PtrArray nulls: 8.
			start _ IntegerVar0.
			firstElem _ 7.
			lastElem _ UInt32Zero.
			elemCount _ 8.
			tally _ UInt32Zero.
			domainIsSimple _ true.!
		*/
	}

	/**
	 * The optional argument just hints at the number of elements
	 * to eventually be added.  It makes no difference semantically.
	 */
	public ActualIntegerTable(IntegerValue size) {
		super();
		if (size.isGT(IntegerValue.zero())) {
			elemCount = size.asInt32();
		} else {
			elemCount = 4;
		}
		elements = PtrArray.make(elemCount);
		start = IntegerValue.zero();
		tally = 0;
		firstElem = elemCount - 1;
		lastElem = 0;
		domainIsSimple = true;
		/*
		udanax-top.st:49886:ActualIntegerTable methodsFor: 'creation'!
		create.IntegerVar: size {IntegerVar} 
			"The optional argument just hints at the number of elements
			 to eventually be added.  It makes no difference semantically."
			super create.
			size > IntegerVar0 ifTrue: [elemCount _ size DOTasLong] ifFalse: [elemCount _ 4].
			elements _ PtrArray nulls: elemCount.
			start _ IntegerVar0.
			tally _ UInt32Zero.
			firstElem _ elemCount - 1.
			lastElem _ UInt32Zero.
			domainIsSimple _ true.!
		*/
	}

	/**
	 * Hint at the domain to be accessed (inclusive, exclusive).
	 */
	public ActualIntegerTable(IntegerValue begin, IntegerValue end) {
		super();
		start = begin;
		elemCount = (end.minus(start)).asInt32();
		if (elemCount < 4) {
			elemCount = 4;
		}
		elements = PtrArray.make(elemCount);
		firstElem = elemCount - 1;
		lastElem = 0;
		tally = 0;
		domainIsSimple = true;
		/*
		udanax-top.st:49899:ActualIntegerTable methodsFor: 'creation'!
		create: begin {IntegerVar} with: end {IntegerVar} 
			"Hint at the domain to be accessed (inclusive, exclusive)."
			super create.
			start _ begin.
			elemCount _ (end - start) DOTasLong.
			elemCount < 4 ifTrue: [elemCount _ 4].
			elements _ PtrArray nulls: elemCount.
			firstElem _ elemCount - 1.
			lastElem _ UInt32Zero.
			tally _ UInt32Zero.
			domainIsSimple _ true!
		*/
	}

	public ActualIntegerTable(PtrArray array, IntegerValue begin, int count, int first, int last, int aTally, boolean simple) {
		super();
		elements = array;
		start = begin;
		elemCount = count;
		firstElem = first;
		lastElem = last;
		tally = aTally;
		domainIsSimple = simple;
		/*
		udanax-top.st:49912:ActualIntegerTable methodsFor: 'creation'!
		create: array {PtrArray} with: begin {IntegerVar} with: count {UInt32} with: first {UInt32} with: last {UInt32} with: aTally {UInt32} with: simple {BooleanVar}
			super create.
			elements  := array.
			start := begin.
			elemCount := count.
			firstElem := first.
			lastElem := last.
			tally := aTally.
			domainIsSimple := simple!
		*/
	}

	public void destroy() {
		if (getNextCOW() == null) {
			super.destroy();
		}
		/*
		udanax-top.st:49922:ActualIntegerTable methodsFor: 'creation'!
		{void} destroy
			self getNextCOW == NULL ifTrue:
				[super destroy]!
		*/
	}

	public ScruTable emptySize(IntegerValue size) {
		return IntegerTable.make((lowestIndex()), (highestIndex().plus(IntegerValue.one())));
		/*
		udanax-top.st:49926:ActualIntegerTable methodsFor: 'creation'!
		{ScruTable} emptySize: size {IntegerVar unused}
			^IntegerTable make.IntegerVar: (self lowestIndex) with: (self highestIndex + 1)!
		*/
	}

	/**
	 * Copy the given range into a new IntegerTable.
	 * The range is startIndex (inclusive) to stopIndex (exclusive)
	 * The first element in the sub table will be at firstIndex
	 */
	public ScruTable offsetSubTableBetween(IntegerValue startIndex, IntegerValue stopIndex, IntegerValue firstIndex) {
		IntegerTable table;
		IntegerValue theEnd;
		theEnd = firstIndex.plus(stopIndex).minus(startIndex).minus(IntegerValue.one());
		table = IntegerTable.make(firstIndex, theEnd);
		for (IntegerValue i = firstIndex; i.isLE(theEnd); i = i.plus(IntegerValue.one())) {
			Heaper val = intFetch(i.plus(startIndex).minus(firstIndex));
			if (val != null) {
				table.atIntIntroduce(i, val);
			}
		}
		return table;
		/*
		udanax-top.st:49930:ActualIntegerTable methodsFor: 'creation'!
		{ScruTable} offsetSubTableBetween: startIndex {IntegerVar} 
			with: stopIndex {IntegerVar} 
			with: firstIndex {IntegerVar} 
			"Copy the given range into a new IntegerTable. 
			The range is startIndex (inclusive) to stopIndex (exclusive)
			The first element in the sub table will be at firstIndex"
			| table {IntegerTable} theEnd {IntegerVar} |
			theEnd _ firstIndex + stopIndex - startIndex - 1.
			table _ IntegerTable make.IntegerVar: firstIndex with: theEnd.
			firstIndex to: theEnd do: 
				[:i {IntegerVar} | 
				| val {Heaper wimpy} |
				val _ self intFetch: i + startIndex - firstIndex.
				val == NULL ifFalse: [table atInt: i introduce: val]].
			^table!
		*/
	}

	public ScruTable subTable(XnRegion reg) {
		IntegerRegion subRegion;
		subRegion = (IntegerRegion) (reg.intersect(domain().asSimpleRegion()));
		if (subRegion.isEmpty()) {
			return emptySize((count().maximum(IntegerValue.one())));
		}
		return subTableBetween(subRegion.start(), subRegion.stop());
		/*
		udanax-top.st:49947:ActualIntegerTable methodsFor: 'creation'!
		{ScruTable} subTable: reg {XnRegion} 
			| subRegion {IntegerRegion} |
			subRegion _ (reg intersect: self domain asSimpleRegion)
						cast: IntegerRegion.
			subRegion isEmpty ifTrue: [^ self emptySize: (self count max: 1)].
			^self subTableBetween: subRegion start with: subRegion stop!
		*/
	}

	/**
	 * Hack for C++ overloading problem
	 */
	public ScruTable subTableBetween(IntegerValue startIndex, IntegerValue stopIndex) {
		return offsetSubTableBetween(startIndex, stopIndex, startIndex);
		/*
		udanax-top.st:49955:ActualIntegerTable methodsFor: 'creation'!
		{ScruTable} subTableBetween: startIndex {IntegerVar} with: stopIndex {IntegerVar}
			"Hack for C++ overloading problem"
			^self offsetSubTableBetween: startIndex with: stopIndex with: startIndex!
		*/
	}

	public XnRegion runAtInt(IntegerValue anIdx) {
		int idx;
		Heaper lastObj;
		boolean notDone;
		idx = (anIdx.minus(start)).asInt32();
		if (tally == 0) {
			return IntegerRegion.make();
		}
		if (idx < firstElem || (idx > lastElem)) {
			return IntegerRegion.make(anIdx, anIdx);
		}
		notDone = true;
		if ((lastObj = elements.fetch(idx)) == null) {
			while (idx <= lastElem && (notDone)) {
				if ((elements.fetch(idx)) == null) {
					idx = idx + 1;
				} else {
					notDone = false;
				}
			}
		} else {
			while (idx <= lastElem && (notDone)) {
				if ((elements.fetch(idx)) != null) {
					if ((elements.fetch(idx)).isEqual(lastObj)) {
						idx = idx + 1;
					} else {
						notDone = false;
					}
				} else {
					notDone = false;
				}
			}
		}
		return IntegerRegion.make(anIdx, (start.plus(IntegerValue.make(idx))));
		/*
		udanax-top.st:49961:ActualIntegerTable methodsFor: 'runs'!
		{XnRegion} runAtInt: anIdx {IntegerVar} 
			| idx {UInt32} lastObj {Heaper} notDone {BooleanVar} |
			idx _ (anIdx - start) DOTasLong.
			tally == UInt32Zero ifTrue: [^ IntegerRegion make].
			(idx < firstElem or: [idx > lastElem])
				ifTrue: [^IntegerRegion make: anIdx with: anIdx].
			notDone _ true.
			(lastObj _ elements fetch: idx) == NULL
				ifTrue: [[idx <= lastElem and: [notDone]]
						whileTrue: [(elements fetch: idx) == NULL
								ifTrue: [idx _ idx + 1]
								ifFalse: [notDone _ false]]]
				ifFalse: [[idx <= lastElem and: [notDone]]
						whileTrue: [(elements fetch: idx) ~~ NULL
								ifTrue: [((elements fetch: idx) isEqual: lastObj)
										ifTrue: [idx _ idx + 1]
										ifFalse: [notDone _ false]]
								ifFalse: [notDone _ false]]].
			^IntegerRegion make: anIdx with: (start + idx)!
		*/
	}

	public void printOn(PrintWriter aStream) {
		aStream.print(getClass().getName());
		printOnWithSimpleSyntax(aStream, "[", ",", "]");
		/*
		udanax-top.st:49983:ActualIntegerTable methodsFor: 'printing'!
		{void} printOn: aStream {ostream reference} 
			aStream << self getCategory name.
			self printOnWithSimpleSyntax: aStream
				with: '['
				with: ','
				with: ']'!
		*/
	}

	/**
	 * ignore order spec for now
	 */
	public TableStepper stepper(OrderSpec order) {
		/* Note that this method depends on the ITAscendingStepper NOT copying the table. */
		if (order == null) {
			if (tally == 0) {
				return IntegerTableStepper.make(this, start, start);
			} else {
				return new ITAscendingStepper(((OberIntegerTable) copy()), start.plus(IntegerValue.make(firstElem))
				/* self lowestIndex */
				, start.plus(IntegerValue.make(lastElem))
				/* self highestIndex */
				);
			}
		} else {
			return IntegerTableStepper.make(this, order);
		}
		/*
		udanax-top.st:49992:ActualIntegerTable methodsFor: 'enumerating'!
		{TableStepper} stepper: order {OrderSpec default: NULL} 
			"ignore order spec for now"
			"Note that this method depends on the ITAscendingStepper NOT copying the table."
			order == NULL
				ifTrue: [tally == UInt32Zero
						ifTrue: [^IntegerTableStepper
								make: self
								with: start
								with: start]
						ifFalse: [^ITAscendingStepper
								create: (self copy cast: OberIntegerTable)
								with: start + firstElem "self lowestIndex"
								with: start + lastElem "self highestIndex"]]
				ifFalse: [^IntegerTableStepper make: self with: order]!
		*/
	}

	public Heaper theOne() {
		if (!count().isEqual(IntegerValue.one())) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_ONE_ELEMENT);
		}
		return intFetch(lowestIndex());
		/*
		udanax-top.st:50007:ActualIntegerTable methodsFor: 'enumerating'!
		{Heaper} theOne
			self count ~~ 1 ifTrue:
				[ Heaper BLAST: #NotOneElement ].
			^ self intFetch: self lowestIndex!
		*/
	}

	public IntegerRegion contigDomainStarting(int anIdx) {
		int begin;
		int tIdx;
		tIdx = begin = anIdx;
		while (tIdx <= lastElem && ((elements.fetch(tIdx)) != null)) {
			tIdx = tIdx + 1;
		}
		if (tIdx > begin) {
			return IntegerRegion.make(start.plus(IntegerValue.make(begin)), start.plus(IntegerValue.make(tIdx)));
		} else {
			return IntegerRegion.make(IntegerValue.make(anIdx), IntegerValue.make(anIdx));
		}
		/*
		udanax-top.st:50014:ActualIntegerTable methodsFor: 'private:'!
		{IntegerRegion} contigDomainStarting: anIdx {UInt32} 
			| begin {UInt32} tIdx {UInt32} |
			tIdx _ begin _ anIdx.
			[tIdx <= lastElem and: [(elements fetch: tIdx) ~~ NULL]]
				whileTrue: [tIdx _ tIdx + 1].
			tIdx > begin
				ifTrue: [^IntegerRegion make: start + begin with: start + tIdx]
				ifFalse: [^IntegerRegion make: anIdx with: anIdx]!
		*/
	}

	/**
	 * return the elements array for rapid processing
	 */
	public PtrArray elementsArray() {
		return elements;
		/*
		udanax-top.st:50023:ActualIntegerTable methodsFor: 'private:'!
		{PtrArray} elementsArray
			"return the elements array for rapid processing"
			^ elements!
		*/
	}

	/**
	 * return the size of the elements array for rapid processing
	 */
	public int endOffset() {
		return lastElem;
		/*
		udanax-top.st:50027:ActualIntegerTable methodsFor: 'private:'!
		{UInt32} endOffset
			"return the size of the elements array for rapid processing"
			^ lastElem!
		*/
	}

	/**
	 * Enlarge the receiver to contain more slots filled with nil.
	 */
	public void enlargeAfter(IntegerValue toMinimum) {
		PtrArray oldElements;
		int tmp;
		int newSize = elemCount * 2;
		if (newSize < 4) {
			newSize = 4;
		}
		if (newSize < (tmp = (toMinimum.minus(start)).asInt32() + 1)) {
			newSize = tmp;
		}
		PtrArray newElements = PtrArray.make(newSize);
		for (int i = 0; i < elemCount; i++) {
			newElements.store(i, (elements.fetch(i)));
		}
		/* Just for the hell of it, I make this robust for asynchronous readers... */
		oldElements = elements;
		elements = newElements;
		oldElements.destroy();
		elemCount = newSize;
		if (tally == 0) {
			firstElem = elements.count() - 1;
		}
		/*
		udanax-top.st:50031:ActualIntegerTable methodsFor: 'private:'!
		{void} enlargeAfter: toMinimum {IntegerVar}
			"Enlarge the receiver to contain more slots filled with nil."
			| newElements{PtrArray} oldElements {PtrArray wimpy} tmp {UInt32} newSize {UInt32} |
			newSize _ elemCount * 2.
			newSize < 4 ifTrue: [newSize _ 4].
			(newSize < (tmp _ (toMinimum - start) DOTasLong + 1)) ifTrue: [newSize _ tmp].
			newElements _ PtrArray nulls: newSize.
			UInt32Zero almostTo: elemCount do: [:i {UInt32} | 
				newElements at: i store: (elements fetch: i)].
			"Just for the hell of it, I make this robust for asynchronous readers..."
			oldElements _ elements.
			elements _ newElements.
			oldElements destroy.
			elemCount _ newSize.
			tally == UInt32Zero ifTrue: [firstElem _ elements count - 1]!
		*/
	}

	/**
	 * Enlarge the receiver to contain more slots filled with nil.
	 */
	public void enlargeBefore(IntegerValue toMinimum) {
		int newSize;
		PtrArray newElements;
		PtrArray oldElements;
		int offset;
		int tmp;
		IntegerValue stop;
		stop = start.plus(IntegerValue.make(elemCount));
		newSize = elemCount * 2;
		if (newSize < 4) {
			newSize = 4;
		}
		if (newSize < (tmp = (stop.minus(toMinimum)).asInt32() + 1)) {
			newSize = tmp;
		}
		newElements = PtrArray.make(newSize);
		offset = newSize - elemCount;
		for (int i = 0; i < elemCount; i++) {
			newElements.store(i + offset, (elements.fetch(i)));
		}
		oldElements = elements;
		elements = newElements;
		oldElements.destroy();
		start = stop.minus(IntegerValue.make(newSize));
		firstElem = firstElem + offset;
		lastElem = lastElem + offset;
		elemCount = newSize;
		/*
		udanax-top.st:50048:ActualIntegerTable methodsFor: 'private:'!
		{void} enlargeBefore: toMinimum {IntegerVar} 
			"Enlarge the receiver to contain more slots filled with nil."
			| newSize {UInt32} newElements {PtrArray} oldElements {PtrArray wimpy} offset {UInt32} tmp {UInt32} stop {IntegerVar} |
			stop _ start + elemCount.
			newSize _ elemCount * 2.
			newSize < 4 ifTrue: [newSize _ 4].
			newSize < (tmp _ (stop - toMinimum) DOTasLong + 1) ifTrue: [newSize _ tmp].
			newElements _ PtrArray nulls: newSize.
			offset _ newSize - elemCount.
			UInt32Zero almostTo: elemCount do: [:i {UInt32} | 
				newElements at: i + offset store: (elements fetch: i)].
			oldElements _ elements.
			elements _ newElements.
			oldElements destroy.
			start _ stop - newSize.
			firstElem _ firstElem + offset.
			lastElem _ lastElem + offset.
			elemCount _ newSize!
		*/
	}

	/**
	 * This method returns the first table entry that is not NULL after index.
	 */
	public int firstElemAfter(int index) {
		int idx;
		if (tally == 0) {
			return elemCount;
		}
		idx = index + 1;
		while ((idx < lastElem) && ((elements.fetch(idx)) == null)) {
			idx = idx + 1;
		}
		return idx;
		/*
		udanax-top.st:50068:ActualIntegerTable methodsFor: 'private:'!
		{UInt32} firstElemAfter: index {UInt32}
			"This method returns the first table entry that is not NULL after index."
			 
			| idx {UInt32} |
			(tally == UInt32Zero) ifTrue: [^elemCount].
			idx _ index + 1.
			[(idx < lastElem) and: [(elements fetch: idx) == NULL]] whileTrue: [idx _ idx + 1].
			^ idx!
		*/
	}

	public IntegerRegion generateDomain() {
		int begin;
		IntegerRegion resReg;
		IntegerRegion nextReg;
		resReg = IntegerRegion.make();
		if (tally == 0) {
			return resReg;
		}
		begin = firstElem;
		while (begin <= lastElem) {
			nextReg = contigDomainStarting(begin);
			if (nextReg.isEmpty()) {
				nextReg = nullDomainStarting(begin);
			} else {
				resReg = (IntegerRegion) (resReg.unionWith(nextReg));
			}
			begin = (nextReg.stop().minus(start)).asInt32();
		}
		return resReg;
		/*
		udanax-top.st:50078:ActualIntegerTable methodsFor: 'private:'!
		{IntegerRegion} generateDomain
			| begin {UInt32} resReg {IntegerRegion} nextReg {IntegerRegion} |
			resReg _ IntegerRegion make.
			tally == UInt32Zero ifTrue: [^resReg].
			begin _ firstElem.
			[begin <= lastElem]
				whileTrue: 
					[nextReg _ self contigDomainStarting: begin.
					nextReg isEmpty
						ifTrue: [nextReg _ self nullDomainStarting: begin]
						ifFalse: [resReg _ (resReg unionWith: nextReg)
										cast: IntegerRegion].
					begin _ (nextReg stop - start) DOTasLong].
			^resReg!
		*/
	}

	/**
	 * This method returns the first table entry that is not NULL after index.
	 */
	public int lastElemBefore(int index) {
		int idx;
		if (tally == 0) {
			return 0;
		}
		idx = index - 1;
		while ((idx > firstElem) && ((elements.fetch(idx)) == null)) {
			idx = idx - 1;
		}
		return idx;
		/*
		udanax-top.st:50093:ActualIntegerTable methodsFor: 'private:'!
		{UInt32} lastElemBefore: index {UInt32}
			"This method returns the first table entry that is not NULL after index."
			 
			| idx {UInt32} |
			(tally == UInt32Zero) ifTrue: [^UInt32Zero].
			idx _ index - 1.
			[(idx > firstElem) and: [(elements fetch: idx) == NULL]] whileTrue: [idx _ idx - 1].
			^ idx!
		*/
	}

	/**
	 * return the size of the elements array for rapid processing
	 */
	public int maxElements() {
		return elemCount;
		/*
		udanax-top.st:50103:ActualIntegerTable methodsFor: 'private:'!
		{UInt32} maxElements
			"return the size of the elements array for rapid processing"
			^ elemCount!
		*/
	}

	public IntegerRegion nullDomainStarting(int anIdx) {
		int begin;
		int tIdx;
		tIdx = begin = anIdx;
		while (tIdx <= lastElem && ((elements.fetch(tIdx)) == null)) {
			tIdx = tIdx + 1;
		}
		if (tIdx > begin) {
			return IntegerRegion.make(start.plus(IntegerValue.make(begin)), start.plus(IntegerValue.make(tIdx)));
		} else {
			return IntegerRegion.make(IntegerValue.make(anIdx), IntegerValue.make(anIdx));
		}
		/*
		udanax-top.st:50107:ActualIntegerTable methodsFor: 'private:'!
		{IntegerRegion} nullDomainStarting: anIdx {UInt32} 
			| begin {UInt32} tIdx {UInt32} |
			tIdx _ begin _ anIdx.
			[tIdx <= lastElem and: [(elements fetch: tIdx) == NULL]]
				whileTrue: [tIdx _ tIdx + 1].
			tIdx > begin
				ifTrue: [^IntegerRegion make: start + begin with: start + tIdx]
				ifFalse: [^IntegerRegion make: anIdx with: anIdx]!
		*/
	}

	/**
	 * return the size of the elements array for rapid processing
	 */
	public IntegerValue startIndex() {
		return start;
		/*
		udanax-top.st:50116:ActualIntegerTable methodsFor: 'private:'!
		{IntegerVar} startIndex
			"return the size of the elements array for rapid processing"
			^ start!
		*/
	}

	/**
	 * return the size of the elements array for rapid processing
	 */
	public int startOffset() {
		return firstElem;
		/*
		udanax-top.st:50120:ActualIntegerTable methodsFor: 'private:'!
		{UInt32} startOffset
			"return the size of the elements array for rapid processing"
			^ firstElem!
		*/
	}

	//	public void fixup() {
	//		super.fixup();
	//		while (((elements.fetch(lastElem)) == null) && (lastElem > 0)) {
	//			System.out.print("d");
	//			lastElem = lastElem - 1;
	//		}
	//		/*
	//		udanax-top.st:50126:ActualIntegerTable methodsFor: 'smalltalk: private:'!
	//		fixup
	//			super fixup.
	//			[((elements fetch: lastElem) == NULL) and: [lastElem > 0]] whileTrue: [
	//				Transcript show: 'd'.
	//				lastElem _ lastElem - 1]!
	//		*/
	//	}

	//	public void inspect() {
	//		return InspectorView.open((IntegerTableInspector.inspect(this)));
	//		/*
	//		udanax-top.st:50132:ActualIntegerTable methodsFor: 'smalltalk: private:'!
	//		{void} inspect
	//			^InspectorView open: (IntegerTableInspector inspect: self)!
	//		*/
	//	}

	public void destruct() {
		elements.destroy();
		elements = null;
		super.destruct();
		/*
		udanax-top.st:50137:ActualIntegerTable methodsFor: 'protected: destruct'!
		{void} destruct
			elements destroy.
			elements _ NULL.
			super destruct!
		*/
	}

	public void becomeCloneOnWrite(Heaper where) {
		//TODO review new
		IntegerTable tmp = new ActualIntegerTable(start, (start.plus(IntegerValue.make(lastElem))));
		if (tally == 0) {
			return;
		}
		TableStepper source = new ITAscendingStepper(this, start.plus(IntegerValue.make(firstElem)), start.plus(IntegerValue.make(lastElem)));
		try {
			Heaper tableElem;
			while ((tableElem = (Heaper) source.fetch()) != null) {
				tmp.atStore(source.position(), tableElem);
				source.step();
			}
		} finally {
			source.destroy();
		}
		/*
		udanax-top.st:50144:ActualIntegerTable methodsFor: 'protected: COW stuff'!
		{void} becomeCloneOnWrite: where {Heaper}
			| tmp {IntegerTable} source {TableStepper} |
			tmp _ (ActualIntegerTable new.Become: where) create: start with: (start + lastElem).
			tally == UInt32Zero ifTrue: [^ VOID].
			source _ ITAscendingStepper create: self with: start + firstElem with: start + lastElem.
			source forEach: [ :tableElem {Heaper} |
				tmp at: source position store: tableElem].!
		*/
	}

	public Heaper atStore(Position key, Heaper value) {
		return atIntStore(((IntegerPos) key).asIntegerVar(), value);
		/*
		udanax-top.st:50154:ActualIntegerTable methodsFor: 'overload junk'!
		{Heaper} at: key {Position} store: value {Heaper} 
			^ self atInt: (key cast: IntegerPos) asIntegerVar store: value!
		*/
	}

	public Heaper fetch(Position key) {
		return intFetch((((IntegerPos) key).asIntegerVar()));
		/*
		udanax-top.st:50158:ActualIntegerTable methodsFor: 'overload junk'!
		{Heaper} fetch: key {Position} 
			^ self intFetch: ((key cast: IntegerPos) asIntegerVar)!
		*/
	}

	public boolean includesKey(Position aKey) {
		return includesIntKey((((IntegerPos) aKey).asIntegerVar()));
		/*
		udanax-top.st:50162:ActualIntegerTable methodsFor: 'overload junk'!
		{BooleanVar} includesKey: aKey {Position}
			^ self includesIntKey: ((aKey cast: IntegerPos) asIntegerVar)!
		*/
	}

	public XnRegion runAt(Position anIdx) {
		return runAtInt((((IntegerPos) anIdx).asIntegerVar()));
		/*
		udanax-top.st:50165:ActualIntegerTable methodsFor: 'overload junk'!
		{XnRegion} runAt: anIdx {Position} 
			^ self runAtInt: ((anIdx cast: IntegerPos) asIntegerVar)!
		*/
	}

	public boolean wipe(Position key) {
		return intWipe((((IntegerPos) key).asIntegerVar()));
		/*
		udanax-top.st:50168:ActualIntegerTable methodsFor: 'overload junk'!
		{BooleanVar} wipe: key {Position}
			^ self intWipe: ((key cast: IntegerPos) asIntegerVar)!
		*/
	}

	public ActualIntegerTable(Rcvr receiver) {
		super(receiver);
		elements = (PtrArray) receiver.receiveHeaper();
		start = receiver.receiveIntegerVar();
		elemCount = receiver.receiveUInt32();
		firstElem = receiver.receiveUInt32();
		lastElem = receiver.receiveUInt32();
		tally = receiver.receiveUInt32();
		domainIsSimple = receiver.receiveBooleanVar();
		/*
		udanax-top.st:50173:ActualIntegerTable methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			elements _ receiver receiveHeaper.
			start _ receiver receiveIntegerVar.
			elemCount _ receiver receiveUInt32.
			firstElem _ receiver receiveUInt32.
			lastElem _ receiver receiveUInt32.
			tally _ receiver receiveUInt32.
			domainIsSimple _ receiver receiveBooleanVar.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(elements);
		xmtr.sendIntegerVar(start);
		xmtr.sendUInt32(elemCount);
		xmtr.sendUInt32(firstElem);
		xmtr.sendUInt32(lastElem);
		xmtr.sendUInt32(tally);
		xmtr.sendBooleanVar(domainIsSimple);
		/*
		udanax-top.st:50183:ActualIntegerTable methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: elements.
			xmtr sendIntegerVar: start.
			xmtr sendUInt32: elemCount.
			xmtr sendUInt32: firstElem.
			xmtr sendUInt32: lastElem.
			xmtr sendUInt32: tally.
			xmtr sendBooleanVar: domainIsSimple.!
		*/
	}
}
