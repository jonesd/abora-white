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
import org.abora.white.exception.AboraRuntimeException;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.Dsp;
import org.abora.white.spaces.basic.Mapping;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.SimpleMapping;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class ConstantMapping extends Mapping {
	protected CoordinateSpace myCoordinateSpace;
	protected XnRegion myValues;
	/*
	udanax-top.st:28866:
	Mapping subclass: #ConstantMapping
		instanceVariableNames: '
			myCoordinateSpace {CoordinateSpace}
			myValues {XnRegion}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces'!
	*/
	/*
	udanax-top.st:28872:
	(ConstantMapping getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	public ConstantMapping(CoordinateSpace cs, XnRegion values) {
		super();
		myCoordinateSpace = cs;
		myValues = values;
		/*
		udanax-top.st:28877:ConstantMapping methodsFor: 'creation'!
		create: cs {CoordinateSpace} with: values {XnRegion}
			super create.
			myCoordinateSpace _ cs.
			myValues _ values!
		*/
	}

	public Position inverseOf(Position pos) {
		throw new AboraRuntimeException(AboraRuntimeException.MULTIPLE_PRE_IMAGES);
		/*
		udanax-top.st:28884:ConstantMapping methodsFor: 'transforming'!
		{Position} inverseOf: pos {Position unused}
			Heaper BLAST: #MultiplePreImages.
			^NULL!
		*/
	}

	public XnRegion inverseOfAll(XnRegion reg) {
		if (reg.intersects(myValues)) {
			return domain();
		} else {
			return coordinateSpace().emptyRegion();
		}
		/*
		udanax-top.st:28889:ConstantMapping methodsFor: 'transforming'!
		{XnRegion} inverseOfAll: reg {XnRegion}
			(reg intersects: myValues)
				ifTrue: [^self domain]
				ifFalse: [^self coordinateSpace emptyRegion]!
		*/
	}

	public Position of(Position pos) {
		if (myValues.isFinite() && (myValues.count().isEqual(IntegerValue.one()))) {
			return myValues.theOne();
		} else {
			throw new AboraRuntimeException(AboraRuntimeException.MULTIPLE_IMAGES);
		}
		/*
		udanax-top.st:28895:ConstantMapping methodsFor: 'transforming'!
		{Position} of: pos {Position unused}
			(myValues isFinite and: [myValues count == 1])
				ifTrue: [^myValues theOne]
				ifFalse: [Heaper BLAST: #MultipleImages].
			^NULL "fodder"!
		*/
	}

	public XnRegion ofAll(XnRegion reg) {
		if (reg.isEmpty()) {
			return rangeSpace().emptyRegion();
		} else {
			return range();
		}
		/*
		udanax-top.st:28902:ConstantMapping methodsFor: 'transforming'!
		{XnRegion} ofAll: reg {XnRegion}
			reg isEmpty
				ifTrue: [^self rangeSpace emptyRegion]
				ifFalse: [^self range]!
		*/
	}

	public Mapping appliedAfter(Dsp dsp) {
		return this;
		/*
		udanax-top.st:28910:ConstantMapping methodsFor: 'accessing'!
		{Mapping} appliedAfter: dsp {Dsp unused}
			^self!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return myCoordinateSpace;
		/*
		udanax-top.st:28914:ConstantMapping methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^ myCoordinateSpace!
		*/
	}

	public XnRegion domain() {
		return myCoordinateSpace.fullRegion();
		/*
		udanax-top.st:28918:ConstantMapping methodsFor: 'accessing'!
		{XnRegion} domain
			
			^myCoordinateSpace fullRegion!
		*/
	}

	public Dsp fetchDsp() {
		return null;
		/*
		udanax-top.st:28922:ConstantMapping methodsFor: 'accessing'!
		{Dsp | NULL} fetchDsp
			^ NULL!
		*/
	}

	public boolean isComplete() {
		return true;
		/*
		udanax-top.st:28925:ConstantMapping methodsFor: 'accessing'!
		{BooleanVar} isComplete
			^true!
		*/
	}

	public boolean isIdentity() {
		return false;
		/*
		udanax-top.st:28929:ConstantMapping methodsFor: 'accessing'!
		{BooleanVar} isIdentity
			^false!
		*/
	}

	public Mapping preCompose(Dsp dsp) {
		return Mapping.make(myCoordinateSpace, (dsp.ofAll(myValues)));
		/*
		udanax-top.st:28933:ConstantMapping methodsFor: 'accessing'!
		{Mapping} preCompose: dsp {Dsp}
			^Mapping make.CoordinateSpace: myCoordinateSpace 
				with.Region: (dsp ofAll: myValues)!
		*/
	}

	public XnRegion range() {
		return myValues;
		/*
		udanax-top.st:28938:ConstantMapping methodsFor: 'accessing'!
		{XnRegion} range
			^myValues!
		*/
	}

	public CoordinateSpace rangeSpace() {
		return myValues.coordinateSpace();
		/*
		udanax-top.st:28941:ConstantMapping methodsFor: 'accessing'!
		{CoordinateSpace} rangeSpace
			^myValues coordinateSpace!
		*/
	}

	public ImmuSet simpleMappings() {
		return ImmuSet.make().with(this);
		/*
		udanax-top.st:28945:ConstantMapping methodsFor: 'accessing'!
		{ImmuSet of: Mapping} simpleMappings
			^ ImmuSet make with: self.!
		*/
	}

	public ImmuSet simpleRegionMappings() {
		return ImmuSet.make().with(this);
		/*
		udanax-top.st:28948:ConstantMapping methodsFor: 'accessing'!
		{ImmuSet of: Mapping} simpleRegionMappings
			^ ImmuSet make with: self.!
		*/
	}

	public Mapping transformedBy(Dsp dsp) {
		return Mapping.make(myCoordinateSpace, (dsp.ofAll(myValues)));
		/*
		udanax-top.st:28952:ConstantMapping methodsFor: 'accessing'!
		{Mapping} transformedBy: dsp {Dsp}
			^Mapping make.CoordinateSpace: myCoordinateSpace 
				with.Region: (dsp ofAll: myValues)!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(myValues);
		oo.print(")");
		/*
		udanax-top.st:28959:ConstantMapping methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << myValues << ')'!
		*/
	}

	public int actualHashForEqual() {
		return myCoordinateSpace.hashForEqual() + myValues.hashForEqual();
		/*
		udanax-top.st:28965:ConstantMapping methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^myCoordinateSpace hashForEqual + myValues hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof ConstantMapping) {
			ConstantMapping cm = (ConstantMapping) other;
			return (cm.coordinateSpace().isEqual(myCoordinateSpace)) && (cm.values().isEqual(myValues));
		} else {
			return false;
		}
		/*
		udanax-top.st:28968:ConstantMapping methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other
				cast: ConstantMapping into: [:cm |
					^(cm coordinateSpace isEqual: myCoordinateSpace)
					 and: [cm values isEqual: myValues]]
				others: [^false].
			^false "fodder"!
		*/
	}

	public XnRegion values() {
		return myValues;
		/*
		udanax-top.st:28978:ConstantMapping methodsFor: 'private: private'!
		{XnRegion} values
			^myValues!
		*/
	}

	public Mapping inverse() {
		return (Mapping.make(rangeSpace(), domainSpace().fullRegion())).restrict(range());
		/*
		udanax-top.st:28983:ConstantMapping methodsFor: 'operations'!
		{Mapping} inverse
			^(Mapping make.CoordinateSpace: self rangeSpace
				with.Region: self domainSpace fullRegion) restrict: self range!
		*/
	}

	public Mapping restrict(XnRegion region) {
		return SimpleMapping.restrictTo(region, this);
		/*
		udanax-top.st:28988:ConstantMapping methodsFor: 'operations'!
		{Mapping} restrict: region {XnRegion}
			^SimpleMapping restrictTo: region with: self!
		*/
	}

	public Mapping restrictRange(XnRegion region) {
		return Mapping.make(myCoordinateSpace, (myValues.intersect(region)));
		/*
		udanax-top.st:28992:ConstantMapping methodsFor: 'operations'!
		{Mapping} restrictRange: region {XnRegion}
			^Mapping make.CoordinateSpace: myCoordinateSpace
				with.Region: (myValues intersect: region)!
		*/
	}

	public Mapping fetchCombine(Mapping aMapping) {
		if (aMapping instanceof ConstantMapping) {
			ConstantMapping cm = (ConstantMapping) aMapping;
			return Mapping.make(coordinateSpace(), (myValues.unionWith(cm.values())));
		} else {
			return null;
		}
		/*
		udanax-top.st:28999:ConstantMapping methodsFor: 'protected'!
		{Mapping} fetchCombine: aMapping {Mapping}
			aMapping 
				cast: ConstantMapping into: [:cm |
					^Mapping make.CoordinateSpace: self coordinateSpace
						with.Region: (myValues unionWith: cm values)]
				others: [^NULL].
			^NULL "fodder"!
		*/
	}

	protected ConstantMapping(Rcvr receiver) {
		super(receiver);
		myCoordinateSpace = (CoordinateSpace) receiver.receiveHeaper();
		myValues = (XnRegion) receiver.receiveHeaper();
		/*
		udanax-top.st:29010:ConstantMapping methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myCoordinateSpace _ receiver receiveHeaper.
			myValues _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myCoordinateSpace);
		xmtr.sendHeaper(myValues);
		/*
		udanax-top.st:29015:ConstantMapping methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myCoordinateSpace.
			xmtr sendHeaper: myValues.!
		*/
	}
}
