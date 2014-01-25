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
package info.dgjones.abora.white.spaces.basic;

import info.dgjones.abora.white.collection.sets.ImmuSet;
import info.dgjones.abora.white.collection.sets.MuSet;
import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.spaces.CompositeMapping;
import info.dgjones.abora.white.spaces.ConstantMapping;
import info.dgjones.abora.white.spaces.EmptyMapping;
import info.dgjones.abora.white.spaces.integers.IntegerPos;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A mapping is a general mapping from one coordinate space to another, with few of the
 * guarantees provided by Dsps.  In particular, the source and destination coordinate spaces
 * can be different, and the mapping doesn''t have to be everywhere defined (but it has to
 * say where it is defined via "domain" and "range" messages).  A mapping doesn''t have to be
 * unique--the same domain position may map to multiple range positions and vice versa.  A
 * mapping of a XuRegion must yield another XuRegion, but a mapping of a simple region
 * doesn''t have to yield a simple region.
 * A useful and valid way to think of a Mapping is as a (possibly infinite) set of pairs (a
 * mathematical set, not a ScruSet).  The domain region consists of the first elements of
 * each pair, and the range region consists of the second elements.
 * A mapping is most useful as a representation of a version comparison of two different
 * organizations of common elements.  The mapping would tell how positions in one
 * organization correspond to positions in the other.
 */
public abstract class Mapping extends Heaper {
	/*
	udanax-top.st:28318:
	Heaper subclass: #Mapping
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:28322:
	Mapping comment:
	'A mapping is a general mapping from one coordinate space to another, with few of the guarantees provided by Dsps.  In particular, the source and destination coordinate spaces can be different, and the mapping doesn''t have to be everywhere defined (but it has to say where it is defined via "domain" and "range" messages).  A mapping doesn''t have to be unique--the same domain position may map to multiple range positions and vice versa.  A mapping of a XuRegion must yield another XuRegion, but a mapping of a simple region doesn''t have to yield a simple region.
		
		A useful and valid way to think of a Mapping is as a (possibly infinite) set of pairs (a mathematical set, not a ScruSet).  The domain region consists of the first elements of each pair, and the range region consists of the second elements.
		
		A mapping is most useful as a representation of a version comparison of two different organizations of common elements.  The mapping would tell how positions in one organization correspond to positions in the other.'!
	*/
	/*
	udanax-top.st:28328:
	(Mapping getOrMakeCxxClassDescription)
		friends:
	'/- friends for class Mapping -/
	friend void storeMapping (Mapping *, MuSet *);
	friend class SimpleMapping;
	';
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:28513:
	Mapping class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:28516:
	(Mapping getOrMakeCxxClassDescription)
		friends:
	'/- friends for class Mapping -/
	friend void storeMapping (Mapping *, MuSet *);
	friend class SimpleMapping;
	';
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected Mapping() {
		super();
	}

	protected Mapping(Rcvr rcvr) {
		super(rcvr);
	}

	/**
	 * the coordinate space of the domain of the Mapping
	 */
	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:28338:Mapping methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		"the coordinate space of the domain of the Mapping"
		self subclassResponsibility!
	*/

	/**
	 * Essential.  region in which it is valid.
	 */
	public abstract XnRegion domain();
	/*
	udanax-top.st:28343:Mapping methodsFor: 'accessing'!
	{XnRegion CLIENT} domain
		"Essential.  region in which it is valid."
		
		self subclassResponsibility!
	*/

	/**
	 * The coordinate space of the domain of the Mapping
	 */
	public CoordinateSpace domainSpace() {
		return coordinateSpace();
		/*
		udanax-top.st:28348:Mapping methodsFor: 'accessing'!
		{CoordinateSpace CLIENT INLINE} domainSpace
			"The coordinate space of the domain of the Mapping"
			^self coordinateSpace!
		*/
	}

	/**
	 * if this is a Dsp or a Dsp retricted to some domain, return the underlying Dsp.  Otherwise
	 * NULL.
	 */
	public abstract Dsp fetchDsp();
	/*
	udanax-top.st:28353:Mapping methodsFor: 'accessing'!
	{Dsp | NULL} fetchDsp
		"if this is a Dsp or a Dsp retricted to some domain, return the underlying Dsp.  Otherwise NULL."
		self subclassResponsibility!
	*/

	/**
	 * Essential. Return true if each Position in the domain is mapped to every Position in the
	 * range.
	 */
	public abstract boolean isComplete();
	//TODO Ravi.thingToDo();
	/* Decide what to do if it is not simple enough */
	/*
	udanax-top.st:28357:Mapping methodsFor: 'accessing'!
	{BooleanVar CLIENT} isComplete
		"Essential. Return true if each Position in the domain is mapped to every Position in the range."
		
		Ravi thingToDo. "Decide what to do if it is not simple enough"
		self subclassResponsibility!
	*/

	/**
	 * Essential. True if this is the identify mapping on the entire space.
	 */
	public abstract boolean isIdentity();
	//TODO Ravi.thingToDo();
	/* Decide about domain */
	/*
	udanax-top.st:28363:Mapping methodsFor: 'accessing'!
	{BooleanVar CLIENT} isIdentity
		"Essential. True if this is the identify mapping on the entire space."
		
		Ravi thingToDo. "Decide about domain"
		self subclassResponsibility!
	*/

	/**
	 * Essential.  region in which inverse is valid.  Same as the region that the domain region
	 * maps to. For you mathematicians, it is the image of the domain under the mapping.
	 */
	public abstract XnRegion range();
	/*
	udanax-top.st:28369:Mapping methodsFor: 'accessing'!
	{XnRegion CLIENT} range
		"Essential.  region in which inverse is valid.  Same as the region that the domain region maps to. For you mathematicians, it is the image of the domain under the mapping."
		
		self subclassResponsibility!
	*/

	/**
	 * The coordinate space of the range of the transformation
	 */
	public abstract CoordinateSpace rangeSpace();
	/*
	udanax-top.st:28374:Mapping methodsFor: 'accessing'!
	{CoordinateSpace CLIENT} rangeSpace
		"The coordinate space of the range of the transformation"
		
		self subclassResponsibility!
	*/

	/**
	 * return a set of simple mappings that would combine to this one
	 */
	public abstract ImmuSet simpleMappings();
	/*
	udanax-top.st:28379:Mapping methodsFor: 'accessing'!
	{ImmuSet of: Mapping} simpleMappings
		"return a set of simple mappings that would combine to this one"
		self subclassResponsibility!
	*/

	/**
	 * return a set of mappings with simple regions as their domains that would combine
	 * to this one.
	 */
	public abstract ImmuSet simpleRegionMappings();
	/*
	udanax-top.st:28383:Mapping methodsFor: 'accessing'!
	{ImmuSet of: Mapping} simpleRegionMappings
		"return a set of mappings with simple regions as their domains that would combine 
		to this one."
		
		self subclassResponsibility!
	*/

	/**
	 * Essential. Break this Mapping up into simpler Mappings which can be combined together to
	 * get this one.
	 */
	public Stepper simplerMappings() {
		return simpleMappings().stepper();
		/*
		udanax-top.st:28389:Mapping methodsFor: 'accessing'!
		{Stepper CLIENT of: Mapping} simplerMappings
			"Essential. Break this Mapping up into simpler Mappings which can be combined together to get this one."
			
			^self simpleMappings stepper!
		*/
	}

	/**
	 * Essential. If this is a 'simpler' Mapping, and not isFull, then return a yet simpleMapping
	 * of some class from which you can get more information. Note that m->restrict
	 * (region)->unrestricted () is not necessarily the same as m, since information may be lost.
	 */
	public Mapping unrestricted() {
		if (fetchDsp() == null) {
			throw new AboraRuntimeException(AboraRuntimeException.NOT_SIMPLE_ENOUGH);
		}
		return fetchDsp();
		/*
		udanax-top.st:28394:Mapping methodsFor: 'accessing'!
		{Mapping CLIENT} unrestricted
			"Essential. If this is a 'simpler' Mapping, and not isFull, then return a yet simpleMapping of some class from which you can get more information. Note that m->restrict (region)->unrestricted () is not necessarily the same as m, since information may be lost."
			
			self fetchDsp == NULL ifTrue:
				[Heaper BLAST: #NotSimpleEnough].
			^self fetchDsp!
		*/
	}

	/**
	 * Inverse transform a position.  Must BLAST if there isn't a unique inverse.
	 * 'a->isEqual (this->of (b))' iff 'b->isEqual (this->inverseOf (a))'.
	 */
	public abstract Position inverseOf(Position after);
	/*
	udanax-top.st:28403:Mapping methodsFor: 'mapping'!
	{Position} inverseOf: after {Position}
		"Inverse transform a position.  Must BLAST if there isn't a unique inverse.
		'a->isEqual (this->of (b))' iff 'b->isEqual (this->inverseOf (a))'."
		self subclassResponsibility!
	*/

	/**
	 * Inverse transform of a region.
	 * 'a->isEqual (this->of (b))' iff 'b->isEqual (this->inverseOf (a))'.
	 */
	public abstract XnRegion inverseOfAll(XnRegion after);
	/*
	udanax-top.st:28409:Mapping methodsFor: 'mapping'!
	{XnRegion} inverseOfAll: after {XnRegion}
		"Inverse transform of a region.
		'a->isEqual (this->of (b))' iff 'b->isEqual (this->inverseOf (a))'."
		self subclassResponsibility!
	*/

	/**
	 * Unboxed version of 'this->inverseOf (xuInteger(pos))'. See
	 * discussion in the XuInteger class comment about boxed and unboxed
	 * protocols
	 */
	public IntegerValue inverseOfInt(IntegerValue pos) {
		return ((IntegerPos) (inverseOf(pos.integer()))).asIntegerVar();
		/*
		udanax-top.st:28415:Mapping methodsFor: 'mapping'!
		{IntegerVar} inverseOfInt: pos {IntegerVar} 
			"Unboxed version of 'this->inverseOf (xuInteger(pos))'. See 
			discussion in the XuInteger class comment about boxed and unboxed 
			protocols"
			^((self inverseOf: pos integer) cast: IntegerPos) asIntegerVar!
		*/
	}

	/**
	 * Transform a position. 'before' must be a Position of my domain space. Iff 'before' is in
	 * the domain region over which I am defined and it maps to a unique range Position then the
	 * result will be that Position. Otherwise BLAST. For example, if I map 1 to 4, 1 to 5, and 2
	 * to 5 (and nothing else), then this method will yield 5 given 2, but BLAST given anything
	 * else. To find all the values 1 maps to, use the 'ofAll' operation on the singleton region
	 * whose member is 1.
	 */
	public abstract Position of(Position before);
	/*
	udanax-top.st:28422:Mapping methodsFor: 'mapping'!
	{Position CLIENT} of: before {Position} 
		"Transform a position. 'before' must be a Position of my domain space. Iff 'before' is in the domain region over which I am defined and it maps to a unique range Position then the result will be that Position. Otherwise BLAST. For example, if I map 1 to 4, 1 to 5, and 2 to 5 (and nothing else), then this method will yield 5 given 2, but BLAST given anything else. To find all the values 1 maps to, use the 'ofAll' operation on the singleton region whose member is 1."
		self subclassResponsibility!
	*/

	/**
	 * Essential.  Transform a region. The result region has exactly those positions which are
	 * the mappings of the positions in 'before'. This must be the case even if these positions
	 * cannot be enumerated. If the mapping for a given position is multiply defined, then (if
	 * that position is in 'before') all position it maps to must be in the result. Because of
	 * this property, the behavior of this method must be taken as really defining the nature of
	 * a particular mapping (with other method's behavior being defined in terms of this one),
	 * despite the fact that it would have been more natural to take Mapping::of(Position *) as
	 * the defining behavior.
	 */
	public abstract XnRegion ofAll(XnRegion before);
	/*
	udanax-top.st:28427:Mapping methodsFor: 'mapping'!
	{XnRegion CLIENT} ofAll: before {XnRegion} 
		"Essential.  Transform a region. The result region has exactly those positions which are the mappings of the positions in 'before'. This must be the case even if these positions cannot be enumerated. If the mapping for a given position is multiply defined, then (if that position is in 'before') all position it maps to must be in the result. Because of this property, the behavior of this method must be taken as really defining the nature of a particular mapping (with other method's behavior being defined in terms of this one), despite the fact that it would have been more natural to take Mapping::of(Position *) as the defining behavior."
		self subclassResponsibility!
	*/

	/**
	 * Unboxed version of 'this->of (xuInteger(pos))'.  See discussion in the XuInteger class
	 * comment about boxed and unboxed protocols
	 */
	public IntegerValue ofInt(IntegerValue pos) {
		return ((IntegerPos) (of(pos.integer()))).asIntegerVar();
		/*
		udanax-top.st:28432:Mapping methodsFor: 'mapping'!
		{IntegerVar} ofInt: pos {IntegerVar}
			"Unboxed version of 'this->of (xuInteger(pos))'.  See discussion in the XuInteger class 
			comment about boxed and unboxed protocols"
			^ ((self of: pos integer) quickCast: IntegerPos) asIntegerVar!
		*/
	}

	/**
	 * Defined by the equivalence:
	 * M->transformedBy(D)->of(R) isEqual (M->of(D->of(R)))
	 * for all regions R in the domainSpace of M. Equivalent to Dsp::compose, except that it is
	 * between a Mapping and a Dsp.
	 */
	public abstract Mapping appliedAfter(Dsp dsp);
	/*
	udanax-top.st:28440:Mapping methodsFor: 'operations'!
	{Mapping} appliedAfter: dsp {Dsp}
		"Defined by the equivalence:
			M->transformedBy(D)->of(R) isEqual (M->of(D->of(R)))
		for all regions R in the domainSpace of M. Equivalent to Dsp::compose, except that it is between a Mapping and a Dsp."
		
		self subclassResponsibility!
	*/

	/**
	 * Essential.  Result will do both mine and other's mappings.  It will do my mapping where I
	 * am defined, and it will do the other's where his is defined.  If we are both defined over
	 * some domain positions, then the result is a multi-valued mapping.  If you think of a
	 * Mapping simply as a set of pairs (see class comment), then 'combine' yields a Mapping
	 * consisting of the union of these two sets.
	 */
	public Mapping combine(Mapping other) {
		Mapping result;
		result = fetchCombine(other);
		if (result != null) {
			return result;
		}
		result = other.fetchCombine(this);
		if (result != null) {
			return result;
		} else {
			MuSet set;
			set = MuSet.make();
			set.store(this);
			set.store(other);
			return CompositeMapping.privateMakeMapping(domainSpace(), rangeSpace(), set.asImmuSet());
		}
		/*
		udanax-top.st:28447:Mapping methodsFor: 'operations'!
		{Mapping CLIENT} combine: other {Mapping}
			"Essential.  Result will do both mine and other's mappings.  It will do my mapping where I am defined, and it will do the other's where his is defined.  If we are both defined over some domain positions, then the result is a multi-valued mapping.  If you think of a Mapping simply as a set of pairs (see class comment), then 'combine' yields a Mapping consisting of the union of these two sets."
			
			| result {Mapping} |
			result _ self fetchCombine: other.
			result ~~ NULL ifTrue: [^result].
			result _ other fetchCombine: self.
			result ~~ NULL
				ifTrue: [^result]
			ifFalse:
				[| set {MuSet of: Mapping} |
				set _ MuSet make.
				set store: self.
				set store: other.
				^CompositeMapping 
					privateMakeMapping: self domainSpace
					with: self rangeSpace
					with: set asImmuSet]!
		*/
	}

	/**
	 * Essential. Return the inverse of this transformation. Considering the Mapping as a set of
	 * pairs (see class comment), return the Dsp which has the mirror image of all my pairs.
	 */
	public abstract Mapping inverse();
	/*
	udanax-top.st:28466:Mapping methodsFor: 'operations'!
	{Mapping CLIENT} inverse
		"Essential. Return the inverse of this transformation. Considering the Mapping as a set of pairs (see class comment), return the Dsp which has the mirror image of all my pairs."
		
		self subclassResponsibility!
	*/

	/**
	 * There is no sensible explanation for what this message does on Mappings
	 * which aren't Dsps.  In the future, we will probably retire this message,
	 * so don't use it.
	 */
	public abstract Mapping preCompose(Dsp dsp);
	/*
	udanax-top.st:28471:Mapping methodsFor: 'operations'!
	{Mapping} preCompose: dsp {Dsp}
		"There is no sensible explanation for what this message does on Mappings 
		which aren't Dsps.  In the future, we will probably retire this message,
		so don't use it."
		self subclassResponsibility!
	*/

	/**
	 * Essential.  Restrict the domain.  The domain of the result will be the intersection of my
	 * domain and 'region'.  Otherwise we are the same.
	 */
	public abstract Mapping restrict(XnRegion region);
	/*
	udanax-top.st:28478:Mapping methodsFor: 'operations'!
	{Mapping CLIENT} restrict: region {XnRegion}
		"Essential.  Restrict the domain.  The domain of the result will be the intersection of my domain and 'region'.  Otherwise we are the same."
		
		self subclassResponsibility!
	*/

	/**
	 * Restrict the range.  The range of the result will be the intersection of my range and
	 * 'region'.  Otherwise we are the same.
	 */
	public abstract Mapping restrictRange(XnRegion region);
	/*
	udanax-top.st:28483:Mapping methodsFor: 'operations'!
	{Mapping} restrictRange: region {XnRegion}
		"Restrict the range.  The range of the result will be the intersection of my range and 'region'.  Otherwise we are the same."
		
		self subclassResponsibility!
	*/

	/**
	 * Defined by the equivalence:
	 * M->transformedBy(D)->of(R) isEqual (D->of(M->of(R)))
	 * for all regions R in the domainSpace of M. Equivalent to Dsp::preCompose, except that it
	 * is between a Mapping and a Dsp.
	 */
	public abstract Mapping transformedBy(Dsp dsp);
	/*
	udanax-top.st:28488:Mapping methodsFor: 'operations'!
	{Mapping} transformedBy: dsp {Dsp}
		"Defined by the equivalence:
			M->transformedBy(D)->of(R) isEqual (D->of(M->of(R)))
		for all regions R in the domainSpace of M. Equivalent to Dsp::preCompose, except that it is between a Mapping and a Dsp."
		
		self subclassResponsibility!
	*/

	/**
	 * if I know how to combine the two into a single mapping, then I do so
	 */
	public abstract Mapping fetchCombine(Mapping mapping);
	/*
	udanax-top.st:28497:Mapping methodsFor: 'vulnerable: accessing'!
	{Mapping} fetchCombine: mapping {Mapping}
		"if I know how to combine the two into a single mapping, then I do so"
		self subclassResponsibility!
	*/

	//	public PrimArray export() {
	//		passe();
	//		/*
	//		udanax-top.st:28503:Mapping methodsFor: 'smalltalk: passe'!
	//		{PrimArray} export
	//			self passe!
	//		*/
	//	}

	public int actualHashForEqual() {
		//TODO review
		return System.identityHashCode(this);
		/*
		udanax-top.st:28509:Mapping methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	/**
	 * Make an empty mapping from cs to rs. The domain will consist of an
	 * empty region in cs, and the range will consist of an empty region in rs
	 */
	public static Mapping make(CoordinateSpace cs, CoordinateSpace rs) {
		return EmptyMapping.make(cs, rs);
		/*
		udanax-top.st:28526:Mapping class methodsFor: 'pseudo constructors'!
		{Mapping INLINE} make.CoordinateSpace: cs {CoordinateSpace} 
			with.CoordinateSpace: rs {CoordinateSpace} 
			"Make an empty mapping from cs to rs. The domain will consist of an 
			empty region in cs, and the range will consist of an empty region in rs"
			^EmptyMapping make: cs with: rs!
		*/
	}

	/**
	 * Make a constant mapping from all positions in cs to all positions in values.
	 */
	public static Mapping make(CoordinateSpace cs, XnRegion values) {
		if (values.isEmpty()) {
			return Mapping.make(cs, values.coordinateSpace());
		} else {
			return new ConstantMapping(cs, values);
		}
		/*
		udanax-top.st:28533:Mapping class methodsFor: 'pseudo constructors'!
		make.CoordinateSpace: cs {CoordinateSpace} with.Region: values {XnRegion}
			"Make a constant mapping from all positions in cs to all positions in values."
			
			values isEmpty
				ifTrue: [^Mapping make.CoordinateSpace: cs
					with.CoordinateSpace: values coordinateSpace]
				ifFalse: [^ConstantMapping create: cs with: values]!
		*/
	}

	/**
	 * The combine of all the mappings in 'mappings'  All domains must be
	 * in cs and all ranges in rs.  cs and rs must be provided in case
	 * 'mappings' is empty.
	 */
	public static Mapping make(CoordinateSpace cs, CoordinateSpace rs, ImmuSet mappings) {
		if (mappings.isEmpty()) {
			return EmptyMapping.make(cs, rs);
		} else {
			MuSet result = MuSet.make();
			Stepper stepper = mappings.stepper();
			try {
				Mapping each;
				while ((each = (Mapping) stepper.fetch()) != null) {
					CompositeMapping.storeMapping(each, result);
					stepper.step();
				}
			} finally {
				stepper.destroy();
			}
			return CompositeMapping.privateMakeMapping(cs, rs, mappings);
		}
		/*
		udanax-top.st:28541:Mapping class methodsFor: 'pseudo constructors'!
		{Mapping} make: cs {CoordinateSpace} 
			with: rs {CoordinateSpace} 
			with: mappings {ImmuSet of: Mapping}
			"The combine of all the mappings in 'mappings'  All domains must be 
			in cs and all ranges in rs.  cs and rs must be provided in case 
			'mappings' is empty."
			
			mappings isEmpty
				ifTrue: [^EmptyMapping make: cs with: rs ]
				ifFalse:
					[| result {MuSet of: Mapping} |
					result _ MuSet make.
					mappings stepper forEach: [ :each {Mapping} |
						CompositeMapping storeMapping: each with: result].
					^CompositeMapping privateMakeMapping: cs with: rs with: mappings]!
		*/
	}

	//	public static Heaper make(Object a, Object b) {
	//		//TODO (CoordinateSpace) a;
	//		if (b instanceof CoordinateSpace) {
	//			return make(a, b);
	//		}
	//		return make(a, ((XnRegion) b));
	//		/*
	//		udanax-top.st:28559:Mapping class methodsFor: 'smalltalk: smalltalk defaults'!
	//		make: a with: b
	//			a cast: CoordinateSpace.
	//			(b isKindOf: CoordinateSpace) ifTrue:
	//				[^self make.CoordinateSpace: a with.CoordinateSpace: b].
	//			^self make.CoordinateSpace: a with.Region: (b cast: XnRegion)!
	//		*/
	//	}

	//	public static Heaper make(XnRegion region, Mapping mapping) {
	//		passe();
	//		/*
	//		udanax-top.st:28567:Mapping class methodsFor: 'smalltalk: passe'!
	//		make.Region: region {XnRegion} with: mapping {Mapping}
	//			self passe!
	//		*/
	//	}

	/**
	 * {Mapping CLIENT} combine: other {Mapping}
	 * {XuRegion CLIENT} domain
	 * {CoordinateSpace CLIENT} domainSpace
	 * {Mapping CLIENT} inverse
	 * {BooleanVar CLIENT} isComplete
	 * {BooleanVar CLIENT} isIdentity
	 * {Position CLIENT} of: before {Position}
	 * {XuRegion CLIENT} ofAll: before {XuRegion}
	 * {XuRegion CLIENT} range
	 * {CoordinateSpace CLIENT} rangeSpace
	 * {Mapping CLIENT} restrict: region {XuRegion}
	 * {Stepper CLIENT of: Mapping} simplerMappings
	 * {Mapping CLIENT} unrestricted
	 */
	public static void info() {
		/*
		udanax-top.st:28572:Mapping class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{Mapping CLIENT} combine: other {Mapping}
		{XuRegion CLIENT} domain
		{CoordinateSpace CLIENT} domainSpace
		{Mapping CLIENT} inverse
		{BooleanVar CLIENT} isComplete
		{BooleanVar CLIENT} isIdentity
		{Position CLIENT} of: before {Position}
		{XuRegion CLIENT} ofAll: before {XuRegion}
		{XuRegion CLIENT} range
		{CoordinateSpace CLIENT} rangeSpace
		{Mapping CLIENT} restrict: region {XuRegion}
		{Stepper CLIENT of: Mapping} simplerMappings
		{Mapping CLIENT} unrestricted
		"!
		*/
	}
}
