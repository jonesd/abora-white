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
package org.abora.white.spaces.integers;

import java.io.PrintWriter;

import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.SpecialistRcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.Dsp;
import org.abora.white.spaces.basic.Mapping;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

/**
 * Transforms integers by adding a (possibly negative) offset.  In addition to the Dsp
 * protocol, an IntegerDsp will respond to "translation" with the offset that it is adding.
 * Old documentation indicated a possibility of a future upgrade of IntegerDsp which would
 * also optionally reflect (or negate) its input in addition to offsetting.  This would
 * however be a non-upwards compatable change in that current clients already assume that the
 * answer to "translation" fully describes the IntegerDsp.  If such a possibility is
 * introduced, it should be as a super-type of IntegerDsp, since it would have a weaker
 * contract.  Then compatability problems can be caught by the type checker.
 */
public class IntegerMapping extends Dsp {
	protected IntegerValue myTranslation;
	protected static IntegerMapping TheIdentityIntegerMapping = new IntegerMapping(IntegerValue.zero());
	/*
	udanax-top.st:29764:
	Dsp subclass: #IntegerMapping
		instanceVariableNames: 'myTranslation {IntegerVar}'
		classVariableNames: 'TheIdentityIntegerMapping {IntegerMapping star} '
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Integers'!
	*/
	/*
	udanax-top.st:29768:
	IntegerMapping comment:
	'Transforms integers by adding a (possibly negative) offset.  In addition to the Dsp protocol, an IntegerDsp will respond to "translation" with the offset that it is adding.  
		
		Old documentation indicated a possibility of a future upgrade of IntegerDsp which would also optionally reflect (or negate) its input in addition to offsetting.  This would however be a non-upwards compatable change in that current clients already assume that the answer to "translation" fully describes the IntegerDsp.  If such a possibility is introduced, it should be as a super-type of IntegerDsp, since it would have a weaker contract.  Then compatability problems can be caught by the type checker.'!
	*/
	/*
	udanax-top.st:29772:
	(IntegerMapping getOrMakeCxxClassDescription)
		friends:
	'/- friends for class IntegerDsp -/
	friend class IntegerSpace;
	';
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
	*/
	/*
	udanax-top.st:29931:
	IntegerMapping class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:29934:
	(IntegerMapping getOrMakeCxxClassDescription)
		friends:
	'/- friends for class IntegerDsp -/
	friend class IntegerSpace;
	';
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #ON.CLIENT; yourself)!
	*/

	/**
	 * Initialize instance variables
	 */
	public IntegerMapping(IntegerValue translation) {
		super();
		myTranslation = translation;
		/*
		udanax-top.st:29781:IntegerMapping methodsFor: 'unprotected for init creation'!
		create: translation {IntegerVar}
			"Initialize instance variables"
			super create.
			myTranslation _ translation.!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(myTranslation);
		oo.print(")");
		/*
		udanax-top.st:29788:IntegerMapping methodsFor: 'printing'!
		{void} printOn: aStream {ostream reference}
			aStream << self getCategory name << '(' << myTranslation << ')'!
		*/
	}

	public Position inverseOf(Position pos) {
		if (pos != null) {
			throw new IllegalStateException();
		}
		/* shouldn't be necessary, but the old code used to check for NULL 
			so I want to make sure I haven't broken anything */
		if (this == TheIdentityIntegerMapping) {
			return pos;
		} else {
			return (((IntegerPos) pos).asIntegerVar().minus(myTranslation)).integer();
		}
		/*
		udanax-top.st:29793:IntegerMapping methodsFor: 'transforming'!
		{Position} inverseOf: pos {Position} 
			(pos ~~ NULL) assert.
			"shouldn't be necessary, but the old code used to check for NULL 
			so I want to make sure I haven't broken anything"
			self == TheIdentityIntegerMapping
				ifTrue: [^pos]
				ifFalse: [^((pos cast: IntegerPos) asIntegerVar - myTranslation) integer]!
		*/
	}

	public XnRegion inverseOfAll(XnRegion reg) {
		IntegerRegion region;
		IntegerEdgeAccumulator result;
		IntegerEdgeStepper edges;
		XnRegion resultReg;
		if (this == TheIdentityIntegerMapping) {
			return reg;
		} else {
			region = (IntegerRegion) reg;
			/* Transform an interval by transforming the endpoints */
			result = IntegerEdgeAccumulator.make(!region.isBoundedBelow(), region.transitionCount());
			edges = region.edgeStepper();
			while (edges.hasValue()) {
				result.edge((inverseOfInt(edges.edge())));
				edges.step();
			}
			edges.destroy();
			resultReg = result.region();
			result.destroy();
			return resultReg;
		}
		/*
		udanax-top.st:29802:IntegerMapping methodsFor: 'transforming'!
		{XnRegion} inverseOfAll: reg {XnRegion} 
			| region {IntegerRegion} result {IntegerEdgeAccumulator} edges {IntegerEdgeStepper} resultReg {XnRegion} |
			self == TheIdentityIntegerMapping
				ifTrue: [^reg]
				ifFalse: 
					[region _ reg cast: IntegerRegion.
					"Transform an interval by transforming the endpoints"
					result _ IntegerEdgeAccumulator make: region isBoundedBelow not with: region transitionCount.
					edges _ region edgeStepper.
					[edges hasValue]
						whileTrue: [result edge: (self inverseOfInt: edges edge).
							edges step].
					edges destroy.
					resultReg _ result region.
					result destroy.
					^ resultReg]!
		*/
	}

	public IntegerValue inverseOfInt(IntegerValue pos) {
		if (this == TheIdentityIntegerMapping) {
			return pos;
		}
		return pos.minus(myTranslation);
		/*
		udanax-top.st:29820:IntegerMapping methodsFor: 'transforming'!
		{IntegerVar} inverseOfInt: pos {IntegerVar}
			self == TheIdentityIntegerMapping ifTrue: [^pos].
			^pos - myTranslation!
		*/
	}

	public Position of(Position pos) {
		if (pos != null) {
			throw new IllegalStateException();
		}
		/* shouldn't be necessary, but the old code used to check for NULL 
			so I want to make sure I haven't broken anything */
		if (this == TheIdentityIntegerMapping) {
			return pos;
		} else {
			return (myTranslation.plus(((IntegerPos) pos).asIntegerVar())).integer();
		}
		/*
		udanax-top.st:29825:IntegerMapping methodsFor: 'transforming'!
		{Position} of: pos {Position} 
			(pos ~~ NULL) assert.
			"shouldn't be necessary, but the old code used to check for NULL 
			so I want to make sure I haven't broken anything"
			
			self == TheIdentityIntegerMapping
				ifTrue: [^pos]
				ifFalse: [^(myTranslation + (pos cast: IntegerPos) asIntegerVar) integer]!
		*/
	}

	public XnRegion ofAll(XnRegion reg) {
		IntegerRegion region;
		IntegerEdgeAccumulator result;
		IntegerEdgeStepper edges;
		XnRegion resultReg;
		if (this == TheIdentityIntegerMapping) {
			return reg;
		} else {
			region = (IntegerRegion) reg;
			/* Transform an interval by transforming the endpoints */
			result = IntegerEdgeAccumulator.make(!region.isBoundedBelow(), region.transitionCount());
			edges = region.edgeStepper();
			while (edges.hasValue()) {
				result.edge((ofInt(edges.edge())));
				edges.step();
			}
			edges.destroy();
			resultReg = result.region();
			result.destroy();
			return resultReg;
		}
		/*
		udanax-top.st:29835:IntegerMapping methodsFor: 'transforming'!
		{XnRegion} ofAll: reg {XnRegion} 
			| region {IntegerRegion} 
			  result {IntegerEdgeAccumulator} 
			  edges {IntegerEdgeStepper}
			  resultReg {XnRegion} |
			  
			self == TheIdentityIntegerMapping
				ifTrue: [^reg]
				ifFalse: 
					[region _ reg cast: IntegerRegion.
					"Transform an interval by transforming the endpoints"
					result _ IntegerEdgeAccumulator 
								make: region isBoundedBelow not 
								with: region transitionCount.
					edges _ region edgeStepper.
					[edges hasValue]
						whileTrue: [result edge: (self ofInt: edges edge).
							edges step].
					edges destroy.
					resultReg _ result region.
					result destroy.
					^ resultReg]!
		*/
	}

	public IntegerValue ofInt(IntegerValue pos) {
		if (this == TheIdentityIntegerMapping) {
			return pos;
		}
		return myTranslation.plus(pos);
		/*
		udanax-top.st:29859:IntegerMapping methodsFor: 'transforming'!
		{IntegerVar} ofInt: pos {IntegerVar}
			self == TheIdentityIntegerMapping ifTrue: [^pos].
			^ myTranslation + pos!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:29866:IntegerMapping methodsFor: 'accessing'!
		{CoordinateSpace INLINE} coordinateSpace
			^ IntegerSpace make!
		*/
	}

	public boolean isIdentity() {
		return myTranslation.isEqual(IntegerValue.zero());
		/*
		udanax-top.st:29870:IntegerMapping methodsFor: 'accessing'!
		{BooleanVar INLINE} isIdentity
			^ myTranslation = IntegerVar0!
		*/
	}

	/**
	 * The offset which I add to a position.
	 * If my translation is 7, then this->of(4) is 11.
	 */
	public IntegerValue translation() {
		return myTranslation;
		/*
		udanax-top.st:29873:IntegerMapping methodsFor: 'accessing'!
		{IntegerVar CLIENT INLINE} translation
			"The offset which I add to a position.  
			If my translation is 7, then this->of(4) is 11."
			
			^myTranslation!
		*/
	}

	public int actualHashForEqual() {
		return myTranslation.asInt32() + getClass().hashCode();
		/*
		udanax-top.st:29881:IntegerMapping methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^ (myTranslation) DOTasLong + #cat.U.IntegerMapping hashForEqual!
		*/
	}

	/**
	 * Should have same offset and reversal
	 */
	public boolean isEqual(Heaper other) {
		if (other instanceof IntegerMapping) {
			IntegerMapping iDsp = (IntegerMapping) other;
			return iDsp.translation() == myTranslation;
		} else {
			return false;
		}
		/*
		udanax-top.st:29884:IntegerMapping methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			"Should have same offset and reversal"
			other
				cast: IntegerMapping into: [:iDsp |
					^iDsp translation = myTranslation]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	public Dsp compose(Dsp other) {
		if (this == TheIdentityIntegerMapping) {
			return other;
		} else {
			if (other == TheIdentityIntegerMapping) {
				return this;
			}
		}
		return IntegerMapping.make((myTranslation.plus(((IntegerMapping) other).translation())));
		/*
		udanax-top.st:29895:IntegerMapping methodsFor: 'combining'!
		{Dsp} compose: other {Dsp} 
			self == TheIdentityIntegerMapping
				ifTrue: [^ other]
				ifFalse: [other == TheIdentityIntegerMapping ifTrue: [^ self]].
			^IntegerMapping make: (myTranslation + (other quickCast: IntegerMapping) translation)!
		*/
	}

	public Mapping inverse() {
		if (this == TheIdentityIntegerMapping) {
			return this;
		}
		return IntegerMapping.make(myTranslation.negated());
		/*
		udanax-top.st:29902:IntegerMapping methodsFor: 'combining'!
		{Mapping} inverse
			self == TheIdentityIntegerMapping ifTrue: [^self].
			^IntegerMapping make: myTranslation negated!
		*/
	}

	public Dsp inverseCompose(Dsp other) {
		if (this == TheIdentityIntegerMapping) {
			return other;
		} else {
			return other.minus(this);
		}
		/*
		udanax-top.st:29907:IntegerMapping methodsFor: 'combining'!
		{Dsp} inverseCompose: other {Dsp}
			self == TheIdentityIntegerMapping
				ifTrue: [ ^ other ]
				ifFalse: [ ^ other minus: self ]!
		*/
	}

	public Dsp minus(Dsp other) {
		if (other == TheIdentityIntegerMapping) {
			return this;
		} else {
			return IntegerMapping.make((myTranslation.minus(((IntegerMapping) other).translation())));
		}
		/*
		udanax-top.st:29913:IntegerMapping methodsFor: 'combining'!
		{Dsp} minus: other {Dsp} 
			other == TheIdentityIntegerMapping
				ifTrue: [ ^self ]
				ifFalse: [ ^IntegerMapping make: (myTranslation - (other cast: IntegerMapping) translation)]!
		*/
	}

	public void sendIntegerMapping(Xmtr xmtr) {
		xmtr.sendIntegerVar(myTranslation);
		/*
		udanax-top.st:29921:IntegerMapping methodsFor: 'sender'!
		{void SEND.HOOK} sendIntegerMapping: xmtr {Xmtr}
			xmtr sendIntegerVar: myTranslation.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		sendIntegerMapping(xmtr);
		/*
		udanax-top.st:29926:IntegerMapping methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			
			self sendIntegerMapping: xmtr.!
		*/
	}

	//public static void initTimeNonInherited() {
	//TheIdentityIntegerMapping = (IntegerMapping.new(PERSISTENT)) new IntegerMapping(IntegerVar.zero());
	///*
	//udanax-top.st:29943:IntegerMapping class methodsFor: 'smalltalk: init'!
	//initTimeNonInherited
	//	TheIdentityIntegerMapping _ (IntegerMapping new.AllocType: #PERSISTENT) create: IntegerVar0!
	//*/
	//}

	//	public static void linkTimeNonInherited() {
	//		TheIdentityIntegerMapping = null;
	//		/*
	//		udanax-top.st:29946:IntegerMapping class methodsFor: 'smalltalk: init'!
	//		linkTimeNonInherited
	//			TheIdentityIntegerMapping _ NULL!
	//		*/
	//	}

	public static IntegerMapping make() {
		return (IntegerMapping) IntegerSpace.make().identityDsp();
		/*
		udanax-top.st:29951:IntegerMapping class methodsFor: 'pseudo constructors'!
		make
			^IntegerSpace make identityDsp cast: IntegerMapping!
		*/
	}

	public static Heaper make(Rcvr rcvr) {
		IntegerValue translate;
		Heaper result;
		translate = rcvr.receiveIntegerVar();
		if (translate == IntegerValue.zero()) {
			result = TheIdentityIntegerMapping;
		} else {
			result = new IntegerMapping(translate);
		}
		((SpecialistRcvr) rcvr).registerIbid(result);
		return result;
		/*
		udanax-top.st:29955:IntegerMapping class methodsFor: 'pseudo constructors'!
		{Heaper} make.Rcvr: rcvr {Rcvr}
			| translate {IntegerVar} result {Heaper} |
			translate _ rcvr receiveIntegerVar.
			translate == IntegerVarZero
				ifTrue: [result _ TheIdentityIntegerMapping]
				ifFalse: [result _ self create: translate].
			(rcvr cast: SpecialistRcvr) registerIbid: result.
			^result!
		*/
	}

	public static IntegerMapping make(IntegerValue translate) {
		if (translate == IntegerValue.zero()) {
			return make();
		} else {
			return new IntegerMapping(translate);
		}
		/*
		udanax-top.st:29964:IntegerMapping class methodsFor: 'pseudo constructors'!
		make: translate {IntegerVar}
			translate == IntegerVar0
				ifTrue: [^self make]
				ifFalse: [^self create: translate]!
		*/
	}

	public static Dsp identity() {
		return new IntegerMapping(IntegerValue.zero());
		/*
		udanax-top.st:29972:IntegerMapping class methodsFor: 'private: for create'!
		{Dsp} identity
			^self create: IntegerVarZero!
		*/
	}

	/**
	 * {IntegerVar CLIENT} translation
	 */
	public static void info() {
		/*
		udanax-top.st:29978:IntegerMapping class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{IntegerVar CLIENT} translation
		"!
		*/
	}
}
