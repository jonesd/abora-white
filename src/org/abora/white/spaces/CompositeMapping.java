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
package org.abora.white.spaces;

import java.io.PrintWriter;

import org.abora.white.collection.sets.ImmuSet;
import org.abora.white.collection.sets.MuSet;
import org.abora.white.collection.sets.SetAccumulator;
import org.abora.white.collection.steppers.Stepper;
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.Dsp;
import org.abora.white.spaces.basic.Mapping;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class CompositeMapping extends Mapping {
	protected CoordinateSpace myCS;
	protected CoordinateSpace myRS;
	protected ImmuSet myMappings;
	/*
	udanax-top.st:28588:
	Mapping subclass: #CompositeMapping
		instanceVariableNames: '
			myCS {CoordinateSpace}
			myRS {CoordinateSpace}
			myMappings {ImmuSet of: Mapping}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces'!
	*/
	/*
	udanax-top.st:28595:
	(CompositeMapping getOrMakeCxxClassDescription)
		friends:
	'/- friends for class CompositeMapping -/
	friend SPTR(Mapping) mapping(Mapping*, Mapping*);
	friend SPTR(Mapping)  privateMakeMapping (CoordinateSpace *, CoordinateSpace *, ImmuSet OF1(Mapping) *);';
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:28831:
	CompositeMapping class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:28834:
	(CompositeMapping getOrMakeCxxClassDescription)
		friends:
	'/- friends for class CompositeMapping -/
	friend SPTR(Mapping) mapping(Mapping*, Mapping*);
	friend SPTR(Mapping)  privateMakeMapping (CoordinateSpace *, CoordinateSpace *, ImmuSet OF1(Mapping) *);';
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	public Mapping appliedAfter(Dsp dsp) {
		SetAccumulator result = SetAccumulator.make();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result.step((each.appliedAfter(dsp)));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), ((ImmuSet) result.value()));
		/*
		udanax-top.st:28604:CompositeMapping methodsFor: 'operations'!
		{Mapping} appliedAfter: dsp {Dsp}
			| result {SetAccumulator of: Mapping} |
			result _ SetAccumulator make.
			myMappings stepper forEach: [ :each {Mapping} |
				result step: (each appliedAfter: dsp)].
			^CompositeMapping
				privateMakeMapping: self coordinateSpace
				with: self rangeSpace
				with: (result value cast: ImmuSet)!
		*/
	}

	public Mapping inverse() {
		//		Ravi.thingToDo(); /* can this be done more efficiently by taking advantage of invariants? */
		Mapping result = Mapping.make(rangeSpace(), domainSpace());
		Stepper stepper = myMappings.stepper();
		try {
			Mapping sub;
			while ((sub = (Mapping) stepper.fetch()) != null) {
				result = result.combine(sub.inverse());
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:28615:CompositeMapping methodsFor: 'operations'!
		{Mapping} inverse
			| result {Mapping} |
			Ravi thingToDo. "can this be done more efficiently by taking advantage of invariants?"
			result := Mapping make.CoordinateSpace: self rangeSpace with: self domainSpace.
			myMappings stepper forEach: [ :sub {Mapping} |
				result := result combine: sub inverse].
			^result!
		*/
	}

	public Mapping preCompose(Dsp dsp) {
		SetAccumulator result = SetAccumulator.make();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result.step((each.preCompose(dsp)));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), ((ImmuSet) result.value()));
		/*
		udanax-top.st:28624:CompositeMapping methodsFor: 'operations'!
		{Mapping} preCompose: dsp {Dsp}
			| result {SetAccumulator of: Mapping} |
			result _ SetAccumulator make.
			myMappings stepper forEach: [ :each {Mapping} |
				result step: (each preCompose: dsp)].
			^CompositeMapping
				privateMakeMapping: self coordinateSpace
				with: self rangeSpace
				with: (result value cast: ImmuSet)!
		*/
	}

	public Mapping restrict(XnRegion region) {
		MuSet result = MuSet.make();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				Mapping restricted = each.restrict(region);
				if (!restricted.domain().isEmpty()) {
					result.store(restricted);
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), result.asImmuSet());
		/*
		udanax-top.st:28635:CompositeMapping methodsFor: 'operations'!
		{Mapping} restrict: region {XnRegion}
			| result {MuSet of: Mapping} |
			result _ MuSet make.
			myMappings stepper forEach: [ :each {Mapping} | 
				| restricted {Mapping} |
				restricted _ each restrict: region.
				restricted domain isEmpty ifFalse:
					[result store: restricted]].
			^CompositeMapping
				privateMakeMapping: self coordinateSpace
				with: self rangeSpace
				with: result asImmuSet!
		*/
	}

	public Mapping restrictRange(XnRegion region) {
		MuSet result = MuSet.make();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				Mapping restricted = each.restrictRange(region);
				if (!restricted.domain().isEmpty()) {
					result.store(restricted);
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), result.asImmuSet());
		/*
		udanax-top.st:28649:CompositeMapping methodsFor: 'operations'!
		{Mapping} restrictRange: region {XnRegion}
			| result {MuSet of: Mapping} |
			result _ MuSet make.
			myMappings stepper forEach: [ :each {Mapping} | 
				| restricted {Mapping} |
				restricted _ each restrictRange: region.
				restricted domain isEmpty ifFalse:
					[result store: restricted]].
			^CompositeMapping
				privateMakeMapping: self coordinateSpace
				with: self rangeSpace
				with: result asImmuSet!
		*/
	}

	public Mapping transformedBy(Dsp dsp) {
		SetAccumulator result = SetAccumulator.make();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result.step((each.transformedBy(dsp)));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return CompositeMapping.privateMakeMapping(coordinateSpace(), rangeSpace(), ((ImmuSet) result.value()));
		/*
		udanax-top.st:28663:CompositeMapping methodsFor: 'operations'!
		{Mapping} transformedBy: dsp {Dsp}
			| result {SetAccumulator of: Mapping} |
			result _ SetAccumulator make.
			myMappings stepper forEach: [ :each {Mapping} |
				result step: (each transformedBy: dsp)].
			^CompositeMapping
				privateMakeMapping: self coordinateSpace
				with: self rangeSpace
				with: (result value cast: ImmuSet)!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return myCS;
		/*
		udanax-top.st:28676:CompositeMapping methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^myCS!
		*/
	}

	public XnRegion domain() {
		XnRegion result = coordinateSpace().emptyRegion();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result = result.unionWith(each.domain());
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:28680:CompositeMapping methodsFor: 'accessing'!
		{XnRegion} domain
			
			| result {XnRegion} |
			result _ self coordinateSpace emptyRegion.
			myMappings stepper forEach: [ :each {Mapping} |
				result _ result unionWith: each domain].
			^result!
		*/
	}

	public Dsp fetchDsp() {
		return null;
		/*
		udanax-top.st:28688:CompositeMapping methodsFor: 'accessing'!
		{Dsp | NULL} fetchDsp
			^NULL!
		*/
	}

	public boolean isComplete() {
		return false
		/* blast? */;
		/*
		udanax-top.st:28691:CompositeMapping methodsFor: 'accessing'!
		{BooleanVar} isComplete
			^false "blast?"!
		*/
	}

	public boolean isIdentity() {
		return false;
		/*
		udanax-top.st:28695:CompositeMapping methodsFor: 'accessing'!
		{BooleanVar} isIdentity
			^false!
		*/
	}

	public XnRegion range() {
		XnRegion result = rangeSpace().emptyRegion();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result = result.unionWith(each.range());
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:28699:CompositeMapping methodsFor: 'accessing'!
		{XnRegion} range
			| result {XnRegion} |
			result _ self rangeSpace emptyRegion.
			myMappings stepper forEach: [ :each {Mapping} |
				result _ result unionWith: each range].
			^result!
		*/
	}

	public CoordinateSpace rangeSpace() {
		return myRS;
		/*
		udanax-top.st:28706:CompositeMapping methodsFor: 'accessing'!
		{CoordinateSpace} rangeSpace
			^myRS!
		*/
	}

	public ImmuSet simpleMappings() {
		return myMappings;
		/*
		udanax-top.st:28710:CompositeMapping methodsFor: 'accessing'!
		{ImmuSet of: Mapping} simpleMappings
			^myMappings!
		*/
	}

	public ImmuSet simpleRegionMappings() {
		Mapping eachSimple;
		MuSet simpleMappings = MuSet.make();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				if (each.domain().isSimple()) {
					simpleMappings.store(each);
				} else {
					Stepper stepperInner = each.domain().simpleRegions();
					try {
						XnRegion simpleRegion;
						while ((simpleRegion = (XnRegion) stepperInner.fetch()) != null) {
							eachSimple = each.restrict(simpleRegion);
							simpleMappings.store(eachSimple);
							stepperInner.step();
						}
					} finally {
						stepperInner.destroy();
					}
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return (ImmuSet.make(simpleMappings));
		/*
		udanax-top.st:28713:CompositeMapping methodsFor: 'accessing'!
		{ImmuSet of: Mapping} simpleRegionMappings
			| simpleMappings {MuSet of: Mapping} eachSimple {Mapping} |
			
			simpleMappings _ MuSet make.
			myMappings stepper forEach: [ :each {Mapping} | 
				each domain isSimple 
					ifTrue:
						[simpleMappings store: each]
					ifFalse:
						[each domain simpleRegions forEach: [:simpleRegion {XnRegion} |
							eachSimple _ each restrict: simpleRegion.
							simpleMappings store: eachSimple]]].
			^(ImmuSet make.MuSet: simpleMappings)!
		*/
	}

	public Position inverseOf(Position pos) {
		Position result = null;
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				if (each.range().hasMember(pos)) {
					if (result == null) {
						result = each.inverseOf(pos);
					} else {
						throw new AboraRuntimeException(AboraRuntimeException.MULTIPLE_PRE_IMAGES);
					}
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_RANGE);
		}
		return result;
		/*
		udanax-top.st:28730:CompositeMapping methodsFor: 'transforming'!
		{Position} inverseOf: pos {Position}
			| result {Position} |
			result _ NULL.
			myMappings stepper forEach: [ :each {Mapping} |
				(each range hasMember: pos) ifTrue:
					[result == NULL
						ifTrue: [result _ each inverseOf: pos]
						ifFalse: [Heaper BLAST: #MultiplePreImages]]].
			result == NULL
				ifTrue: [Heaper BLAST: #NotInRange].
			^result!
		*/
	}

	public XnRegion inverseOfAll(XnRegion reg) {
		XnRegion result = coordinateSpace().emptyRegion();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result = result.unionWith((each.inverseOfAll(reg)));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:28743:CompositeMapping methodsFor: 'transforming'!
		{XnRegion} inverseOfAll: reg {XnRegion}
			| result {XnRegion} |
			result _ self coordinateSpace emptyRegion.
			myMappings stepper forEach: [ :each {Mapping} |
				result _ result unionWith: (each inverseOfAll: reg)].
			^result!
		*/
	}

	public Position of(Position pos) {
		Position result = null;
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				if (each.domain().hasMember(pos)) {
					if (result == null) {
						result = each.of(pos);
					} else {
						throw new AboraRuntimeException(AboraRuntimeException.MULTIPLE_IMAGES);
					}
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		if (result == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_IN_DOMAIN);
		}
		return result;
		/*
		udanax-top.st:28751:CompositeMapping methodsFor: 'transforming'!
		{Position} of: pos {Position}
			| result {Position} |
			result _ NULL.
			myMappings stepper forEach: [ :each {Mapping} |
				(each domain hasMember: pos) ifTrue:
					[result == NULL
						ifTrue: [result _ each of: pos]
						ifFalse: [Heaper BLAST: #MultipleImages]]].
			result == NULL
				ifTrue: [Heaper BLAST: #NotInDomain].
			^result!
		*/
	}

	public XnRegion ofAll(XnRegion reg) {
		XnRegion result = rangeSpace().emptyRegion();
		Stepper stepper = myMappings.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				result = result.unionWith((each.ofAll(reg)));
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		return result;
		/*
		udanax-top.st:28764:CompositeMapping methodsFor: 'transforming'!
		{XnRegion} ofAll: reg {XnRegion}
			| result {XnRegion} |
			result _ self rangeSpace emptyRegion.
			myMappings stepper forEach: [ :each {Mapping} |
				result _ result unionWith: (each ofAll: reg)].
			^result!
		*/
	}

	public void printOn(PrintWriter stream) {
		stream.print(getClass().getName());
		myMappings.printOnWithSimpleSyntax(stream, "(", ", ", ")");
		/*
		udanax-top.st:28774:CompositeMapping methodsFor: 'printing'!
		{void} printOn: stream {ostream reference}
			stream << self getCategory name.
			myMappings printOnWithSimpleSyntax: stream with: '(' with: ', ' with: ')'!
		*/
	}

	public CompositeMapping(CoordinateSpace cs, CoordinateSpace rs, ImmuSet mappings) {
		super();
		myCS = cs;
		myRS = rs;
		myMappings = mappings;
		/*
		udanax-top.st:28781:CompositeMapping methodsFor: 'private: private creation'!
		create: cs {CoordinateSpace} with: rs {CoordinateSpace} with: mappings {ImmuSet of: Mapping}
			super create.
			myCS _ cs.
			myRS _ rs.
			myMappings _ mappings!
		*/
	}

	public int actualHashForEqual() {
		return getClass().hashCode() ^ myMappings.hashForEqual();
		/*
		udanax-top.st:28789:CompositeMapping methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^#cat.U.CompositeMapping hashForEqual bitXor: myMappings hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof CompositeMapping) {
			CompositeMapping cm = (CompositeMapping) other;
			return cm.simpleMappings().isEqual(myMappings);
		} else {
			return false;
		}
		/*
		udanax-top.st:28792:CompositeMapping methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: CompositeMapping into: [:cm |
					^cm simpleMappings isEqual: myMappings]
				others: [^false].
			^false "fodder"!
		*/
	}

	public Mapping fetchCombine(Mapping mapping) {
		if (mapping instanceof EmptyMapping) {
			return this;
		} else {
			MuSet result = myMappings.asMuSet();
			if (mapping instanceof CompositeMapping) {
				Stepper stepper = mapping.simpleMappings().stepper();
				try {
					Mapping each;
					while ((each = (Mapping) stepper.fetch()) != null) {
						CompositeMapping.storeMapping(each, result);
						stepper.step();
					}
				} finally {
					stepper.destroy();
				}
			} else {
				CompositeMapping.storeMapping(mapping, result);
			}
			return CompositeMapping.privateMakeMapping(myCS, myRS, result.asImmuSet());
		}
		/*
		udanax-top.st:28802:CompositeMapping methodsFor: 'protected: protected'!
		{Mapping} fetchCombine: mapping {Mapping}
			(mapping isKindOf: EmptyMapping)
				ifTrue: [  ^ self ]
				ifFalse:
					[| result {MuSet of: Mapping} |
					result _ myMappings asMuSet.
					(mapping isKindOf: CompositeMapping) ifTrue:
						[mapping simpleMappings stepper forEach: [ :each {Mapping} |
							CompositeMapping storeMapping: each with: result]]
						ifFalse:
							[CompositeMapping storeMapping: mapping with: result].
					^CompositeMapping privateMakeMapping: myCS with: myRS with: result asImmuSet]!
		*/
	}

	protected CompositeMapping(Rcvr receiver) {
		super(receiver);
		myCS = (CoordinateSpace) receiver.receiveHeaper();
		myRS = (CoordinateSpace) receiver.receiveHeaper();
		myMappings = (ImmuSet) receiver.receiveHeaper();
		/*
		udanax-top.st:28818:CompositeMapping methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myCS _ receiver receiveHeaper.
			myRS _ receiver receiveHeaper.
			myMappings _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myCS);
		xmtr.sendHeaper(myRS);
		xmtr.sendHeaper(myMappings);
		/*
		udanax-top.st:28824:CompositeMapping methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myCS.
			xmtr sendHeaper: myRS.
			xmtr sendHeaper: myMappings.!
		*/
	}

	public static Mapping privateMakeMapping(CoordinateSpace cs, CoordinateSpace rs, ImmuSet mappings) {
		if (mappings.isEmpty()) {
			return EmptyMapping.make(cs, rs);
		} else {
			if (mappings.count().isEqual(IntegerValue.one())) {
				return (Mapping) mappings.theOne();
			} else {
				return new CompositeMapping(cs, rs, mappings);
			}
		}
		/*
		udanax-top.st:28843:CompositeMapping class methodsFor: 'functions'!
		{Mapping} privateMakeMapping: cs {CoordinateSpace} 
			with: rs {CoordinateSpace} 
			with: mappings {ImmuSet of: Mapping}
			
			mappings isEmpty
				ifTrue: [^EmptyMapping make: cs with: rs]
				ifFalse:
					[mappings count = 1
						ifTrue: [^mappings theOne cast: Mapping]
						ifFalse: [^CompositeMapping create: cs with: rs with: mappings]]!
		*/
	}

	/**
	 * store a map into the set, checking to see if it can be combined with another
	 */
	public static void storeMapping(Mapping map, MuSet maps) {
		Stepper stepper = maps.stepper();
		try {
			Mapping each;
			while ((each = (Mapping) stepper.fetch()) != null) {
				Mapping combined = map.fetchCombine(each);
				if (combined != null) {
					combined = each.fetchCombine(map);
				}
				if (combined != null) {
					maps.remove(each);
					maps.introduce(combined);
					return;
				}
				stepper.step();
			}
		} finally {
			stepper.destroy();
		}
		maps.introduce(map);
		/*
		udanax-top.st:28854:CompositeMapping class methodsFor: 'functions'!
		{void} storeMapping: map {Mapping} with: maps {MuSet of: Mapping}
			"store a map into the set, checking to see if it can be combined with another"
			maps stepper forEach: [ :each {Mapping} | | combined {Mapping} |
				combined _ map fetchCombine: each.
				combined ~~ NULL ifTrue:
					[combined _ each fetchCombine: map].
				combined ~~ NULL ifTrue:
					[maps remove: each.
					maps introduce: combined.
					^VOID]].
			maps introduce: map!
		*/
	}
}
