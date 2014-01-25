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
package info.dgjones.abora.white.tumbler;

import info.dgjones.abora.white.collection.arrays.IntegerVarArray;
import info.dgjones.abora.white.collection.arrays.PrimArray;
import info.dgjones.abora.white.collection.arrays.PrimIntegerArray;
import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.SpecialistRcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * The space of all Sequences
 */
public class SequenceSpace extends CoordinateSpace {
	protected static SequenceSpace TheSequenceSpace;
	/*
	udanax-top.st:15642:
	CoordinateSpace subclass: #SequenceSpace
		instanceVariableNames: ''
		classVariableNames: 'TheSequenceSpace {SequenceSpace} '
		poolDictionaries: ''
		category: 'Xanadu-tumbler'!
	*/
	/*
	udanax-top.st:15646:
	SequenceSpace comment:
	'The space of all Sequences'!
	*/
	/*
	udanax-top.st:15648:
	(SequenceSpace getOrMakeCxxClassDescription)
		friends:
	'/- friends for class SequenceSpace -/
	friend class Sequence;
	';
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
	*/
	/*
	udanax-top.st:15798:
	SequenceSpace class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:15801:
	(SequenceSpace getOrMakeCxxClassDescription)
		friends:
	'/- friends for class SequenceSpace -/
	friend class Sequence;
	';
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
	*/

	public SequenceSpace() {
		super(
			(SequenceRegion.usingx(false, PtrArray.empty())),
			(SequenceRegion.usingx(true, PtrArray.empty())),
			(SequenceMapping.make(IntegerValue.zero(), Sequence.zero())),
			SequenceUpOrder.make());
		/*
		udanax-top.st:15658:SequenceSpace methodsFor: 'create'!
		create
			
			super create: (SequenceRegion usingx: false with: PtrArray empty)
				with: (SequenceRegion usingx: true with: PtrArray empty)
				with: (SequenceMapping make: IntegerVarZero with: Sequence zero)
				with: SequenceUpOrder make!
		*/
	}

	public Sequence position(PrimArray numbers) {
		return position(numbers, IntegerValue.zero());
		/*
		udanax-top.st:15667:SequenceSpace methodsFor: 'temporary'!
		{Sequence CLIENT login INLINE} position: numbers {PrimArray}
			^self position: numbers with: IntegerVarZero!
		*/
	}

	/**
	 * Essential. All sequences >= sequence if inclusive, > sequence if not.
	 */
	public SequenceRegion above(Sequence sequence, boolean inclusive) {
		if (inclusive) {
			return SequenceRegion.usingx(false, ((PtrArray) (PrimSpec.pointer().arrayWith((BeforeSequence.make(sequence))))));
		} else {
			return SequenceRegion.usingx(false, ((PtrArray) (PrimSpec.pointer().arrayWith((AfterSequence.make(sequence))))));
		}
		/*
		udanax-top.st:15673:SequenceSpace methodsFor: 'making'!
		{SequenceRegion CLIENT} above: sequence {Sequence} with: inclusive {BooleanVar}
			"Essential. All sequences >= sequence if inclusive, > sequence if not."
			
			inclusive ifTrue:
				[^SequenceRegion usingx: false
					with: ((PrimSpec pointer arrayWith: (BeforeSequence make: sequence)) cast: PtrArray)]
			ifFalse:
				[^SequenceRegion usingx: false
					with: ((PrimSpec pointer arrayWith: (AfterSequence make: sequence)) cast: PtrArray)]!
		*/
	}

	/**
	 * Essential. All sequences <= sequence if inclusive, < sequence if not.
	 */
	public SequenceRegion below(Sequence sequence, boolean inclusive) {
		if (inclusive) {
			return SequenceRegion.usingx(true, ((PtrArray) (PrimSpec.pointer().arrayWith((AfterSequence.make(sequence))))));
		} else {
			return SequenceRegion.usingx(true, ((PtrArray) (PrimSpec.pointer().arrayWith((BeforeSequence.make(sequence))))));
		}
		/*
		udanax-top.st:15683:SequenceSpace methodsFor: 'making'!
		{SequenceRegion CLIENT} below: sequence {Sequence} with: inclusive {BooleanVar}
			"Essential. All sequences <= sequence if inclusive, < sequence if not."
			
			inclusive ifTrue:
				[^SequenceRegion usingx: true
					with: ((PrimSpec pointer arrayWith: (AfterSequence make: sequence)) cast: PtrArray)]
			ifFalse:
				[^SequenceRegion usingx: true
					with: ((PrimSpec pointer arrayWith: (BeforeSequence make: sequence)) cast: PtrArray)]!
		*/
	}

	/**
	 * Return a region of all sequence >= lower and < upper.
	 */
	public SequenceRegion interval(Sequence start, Sequence stop) {
		/* Ravi thingToDo. */
		/* use a single constructor */
		/* Performance */
		return (SequenceRegion) ((above(start, true)).intersect((below(stop, false))));
		/*
		udanax-top.st:15693:SequenceSpace methodsFor: 'making'!
		{SequenceRegion CLIENT} interval: start {Sequence} with: stop {Sequence}
			"Return a region of all sequence >= lower and < upper."
			
			"Ravi thingToDo." "use a single constructor" "Performance"
			^((self above: start with: true)
				intersect: (self below: stop with: false)) cast: SequenceRegion!
		*/
	}

	/**
	 * A transformation which shifts a value by some number of places and then adds a translation
	 * to it.
	 */
	public SequenceMapping mapping(IntegerValue shift, Sequence translation) {
		//		TODO thingToDo(); /* better name for this method */
		if (translation == null) {
			return SequenceMapping.make(shift, Sequence.zero());
		}
		return SequenceMapping.make(shift, translation);
		/*
		udanax-top.st:15700:SequenceSpace methodsFor: 'making'!
		{SequenceMapping CLIENT} mapping: shift {IntegerVar}
			with: translation {Sequence default: NULL}
			"A transformation which shifts a value by some number of places and then adds a translation to it."
			
			self thingToDo. "better name for this method"
			translation == NULL ifTrue:
				[^SequenceMapping make: shift with: Sequence zero].
			^SequenceMapping make: shift with: translation!
		*/
	}

	/**
	 * Essential. A sequence using the given numbers and shift. Leading and trailing zeros will
	 * be stripped, and a copy will be made so that noone modifies it
	 */
	public Sequence position(PrimArray arg, IntegerValue shift) {
		/* IntegerVars cannot have default arguments */
		PrimIntegerArray numbers;
		numbers = (PrimIntegerArray) arg;
		if (numbers == null) {
			return Sequence.usingx(shift, (IntegerVarArray.make(0)));
		}
		return Sequence.usingx(shift, ((PrimIntegerArray) numbers.copy()));
		/*
		udanax-top.st:15709:SequenceSpace methodsFor: 'making'!
		{Sequence CLIENT login} position: arg {PrimArray}
			with: shift {IntegerVar}
			"Essential. A sequence using the given numbers and shift. Leading and trailing zeros will be stripped, and a copy will be made so that noone modifies it"
			"IntegerVars cannot have default arguments"
			
			| numbers {PrimIntegerArray} |
			numbers _ arg cast: PrimIntegerArray.
			numbers == NULL ifTrue:
				[^Sequence usingx: shift with: (IntegerVarArray zeros: Int32Zero)].
			^Sequence usingx: shift with: (numbers copy cast: PrimIntegerArray)!
		*/
	}

	/**
	 * Essential. All sequences which match the given one up to and including the given index.
	 */
	public SequenceRegion prefixedBy(Sequence sequence, IntegerValue limit) {
		return SequenceRegion.usingx(
			false,
			((PtrArray) (PrimSpec
				.pointer()
				.arrayWithTwo((BeforeSequencePrefix.below(sequence, limit)), (BeforeSequencePrefix.above(sequence, limit))))));
		/*
		udanax-top.st:15720:SequenceSpace methodsFor: 'making'!
		{SequenceRegion CLIENT} prefixedBy: sequence {Sequence}
			with: limit {IntegerVar}
			"Essential. All sequences which match the given one up to and including the given index."
			
			^SequenceRegion usingx: false
				with: ((PrimSpec pointer
					arrayWithTwo: (BeforeSequencePrefix below: sequence with: limit)
					with: (BeforeSequencePrefix above: sequence with: limit)) cast: PtrArray)!
		*/
	}

	//	public Sequence sequence(PrimIntegerArray numbers, IntegerVar shift) {
	//		passe()
	//		/* position */;
	//		/*
	//		udanax-top.st:15731:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{Sequence} sequence: numbers {PrimIntegerArray | NULL}
	//			with: shift {IntegerVar | IntegerVarZero}
	//			self passe "position"!
	//		*/
	//	}

	/**
	 * Essential. All sequences greater than or equal to the given sequence.
	 * Should this just be supplanted by CoordinateSpace::region ()?
	 */
	//	public SequenceRegion sequencesAfter(Sequence sequence) {
	//		passe();
	//		return SequenceRegion.usingx(false, (PrimSpec.pointer().arrayWith((BeforeSequence.make(sequence)))));
	//		/*
	//		udanax-top.st:15736:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{SequenceRegion} sequencesAfter: sequence {Sequence}
	//			"Essential. All sequences greater than or equal to the given sequence.
	//			Should this just be supplanted by CoordinateSpace::region ()?"
	//			self passe.
	//			^SequenceRegion usingx: false
	//				with: (PrimSpec pointer arrayWith: (BeforeSequence make: sequence))!
	//		*/
	//	}

	//	/**
	//	 * Essential. All sequences less than or equal to the given sequence.
	//	 * Should this just be supplanted by CoordinateSpace::region ()?
	//	 */
	//	public SequenceRegion sequencesBefore(Sequence sequence) {
	//		passe();
	//		return SequenceRegion.usingx(true, (PrimSpec.pointer().arrayWith((AfterSequence.make(sequence)))));
	//		/*
	//		udanax-top.st:15743:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{SequenceRegion} sequencesBefore: sequence {Sequence}
	//			"Essential. All sequences less than or equal to the given sequence.
	//			Should this just be supplanted by CoordinateSpace::region ()?"
	//			
	//			self passe.
	//			^SequenceRegion usingx: true
	//				with: (PrimSpec pointer arrayWith: (AfterSequence make: sequence))!
	//		*/
	//	}

	//	/**
	//	 * Essential. All sequences which match the given one up to and including the given index.
	//	 * Should this just be supplanted by CoordinateSpace::region ()?
	//	 */
	//	public SequenceRegion sequencesPrefixedBy(Sequence sequence, IntegerVar limit) {
	//		passe();
	//		return SequenceRegion.usingx(
	//			false,
	//			(PrimSpec.pointer().arrayWithTwo((BeforeSequencePrefix.below(sequence, limit)), (BeforeSequencePrefix.above(sequence, limit)))));
	//		/*
	//		udanax-top.st:15751:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{SequenceRegion} sequencesPrefixedBy: sequence {Sequence}
	//			with: limit {IntegerVar}
	//			"Essential. All sequences which match the given one up to and including the given index.
	//			Should this just be supplanted by CoordinateSpace::region ()?"
	//			self passe.
	//			^SequenceRegion usingx: false
	//				with: (PrimSpec pointer
	//					arrayWithTwo: (BeforeSequencePrefix below: sequence with: limit)
	//					with: (BeforeSequencePrefix above: sequence with: limit))!
	//		*/
	//	}

	//	public SequenceMapping shiftAndTranslation() {
	//		passe();
	//		/*
	//		udanax-top.st:15761:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{SequenceMapping} shiftAndTranslation
	//			self passe!
	//		*/
	//	}

	//	public SequenceDsp shiftAndTranslation(IntegerVar shift) {
	//		passe();
	//		/*
	//		udanax-top.st:15765:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{SequenceDsp} shiftAndTranslation: shift {IntegerVar}
	//			self passe!
	//		*/
	//	}

	//	public SequenceDsp shiftAndTranslation(IntegerVar shift, Sequence translation) {
	//		passe();
	//		/*
	//		udanax-top.st:15769:SequenceSpace methodsFor: 'smalltalk: passe'!
	//		{SequenceDsp} shiftAndTranslation: shift {IntegerVar}
	//			with: translation {Sequence}
	//			self passe!
	//		*/
	//	}

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
		//		return getCategory().hashForEqual() + 1;
		/*
		udanax-top.st:15776:SequenceSpace methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			"is equal to any basic space on the same category of positions"
			^self getCategory hashForEqual + 1!
		*/
	}

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public boolean isEqual(Heaper anObject) {
		return anObject.getClass() == getClass();
		/*
		udanax-top.st:15781:SequenceSpace methodsFor: 'testing'!
		{BooleanVar} isEqual: anObject {Heaper}
			"is equal to any basic space on the same category of positions"
			^anObject getCategory == self getCategory!
		*/
	}

	/**
	 * A transformation which shifts a value by some number of places and then adds a translation
	 * to it.
	 */
	public SequenceMapping mapping(IntegerValue shift) {
		return mapping(shift, null);
		/*
		udanax-top.st:15788:SequenceSpace methodsFor: 'smalltalk: defaults'!
		{SequenceMapping CLIENT} mapping: shift {IntegerVar}
			"A transformation which shifts a value by some number of places and then adds a translation to it."
			
			^self mapping: shift with: NULL!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		/*
		udanax-top.st:15795:SequenceSpace methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}!
		*/
	}

	public static Heaper make(Rcvr rcvr) {
		((SpecialistRcvr) rcvr).registerIbid(TheSequenceSpace);
		return TheSequenceSpace;
		/*
		udanax-top.st:15811:SequenceSpace class methodsFor: 'rcvr creation'!
		{Heaper} make.Rcvr: rcvr {Rcvr}
			(rcvr cast: SpecialistRcvr) registerIbid: TheSequenceSpace.
			^TheSequenceSpace!
		*/
	}

	/**
	 * Get the receiver for wire requests.
	 */
	public static SequenceSpace implicitReceiver() {
		return TheSequenceSpace;
		/*
		udanax-top.st:15817:SequenceSpace class methodsFor: 'creation'!
		{SequenceSpace INLINE} implicitReceiver
			"Get the receiver for wire requests."
			^TheSequenceSpace!
		*/
	}

	public static SequenceSpace make() {
		return TheSequenceSpace;
		/*
		udanax-top.st:15821:SequenceSpace class methodsFor: 'creation'!
		{SequenceSpace CLIENT login INLINE} make
			^TheSequenceSpace!
		*/
	}

	public static void initTimeNonInherited() {
		//		REQUIRES(Sequence.getCategory());
		TheSequenceSpace = new SequenceSpace();
		/*
		udanax-top.st:15826:SequenceSpace class methodsFor: 'smalltalk: init'!
		initTimeNonInherited
			self REQUIRES: Sequence.
			TheSequenceSpace := self create!
		*/
	}

	public static void linkTimeNonInherited() {
		TheSequenceSpace = null;
		/*
		udanax-top.st:15831:SequenceSpace class methodsFor: 'smalltalk: init'!
		linkTimeNonInherited
			TheSequenceSpace := NULL!
		*/
	}

	/**
	 * {SequenceRegion CLIENT} above: sequence {Sequence} with: inclusive {BooleanVar}
	 * {SequenceRegion CLIENT} below: sequence {Sequence} with: inclusive {BooleanVar}
	 * {SequenceRegion CLIENT} interval: lower {Region} with: upper {Sequence}
	 * {SequenceMapping CLIENT} mapping: shift {IntegerVar} with: translation {Sequence}
	 * {Sequence CLIENT} position: numbers {PrimIntegerArray}
	 * {Sequence CLIENT} position: numbers {PrimIntegerArray | NULL} with: shift {IntegerVar |
	 * IntegerVarZero}
	 * {SequenceRegion CLIENT} prefixedBy: sequence {Sequence} with: limit {IntegerVar}
	 */
	public static void info() {
		/*
		udanax-top.st:15837:SequenceSpace class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{SequenceRegion CLIENT} above: sequence {Sequence} with: inclusive {BooleanVar}
		{SequenceRegion CLIENT} below: sequence {Sequence} with: inclusive {BooleanVar}
		{SequenceRegion CLIENT} interval: lower {Region} with: upper {Sequence}
		{SequenceMapping CLIENT} mapping: shift {IntegerVar} with: translation {Sequence}
		{Sequence CLIENT} position: numbers {PrimIntegerArray}
		{Sequence CLIENT} position: numbers {PrimIntegerArray | NULL} with: shift {IntegerVar | IntegerVarZero}
		{SequenceRegion CLIENT} prefixedBy: sequence {Sequence} with: limit {IntegerVar}
		"!
		*/
	}
}
