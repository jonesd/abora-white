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
package info.dgjones.abora.white.spaces.unordered;

import java.io.PrintWriter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.Mapping;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * An implementation sharing convenience for Dsp classes which only provide the identity
 * mapping functionality for their coordinate spaces.  This provides everything except the
 * coordinate space itself (which must be provided by the subclass).  Will eventually be
 * declared NOT_A_TYPE, so don''t use it in type declarations.
 * <p>
 * Assumes that if a given space uses it as its identity Dsp, then the one cached instance
 * will be the only identity Dsp for that space.  I.e., I do equality comparison as an EQ
 * object.  If this assumpsion isn''t true, please override isEqual and hashForEqual.  See
 * PathDsp.
 * <p>
 * IdentityDsp is in module "unorder" because typically unordered spaces will only have an
 * identity Dsp and so want to subclass this class.  Non-unordered spaces should also feel
 * free to use this as appropriate.
 */
public abstract class IdentityDsp extends Dsp {
//	private static IdentityDsp theDsp;
	/*
	udanax-top.st:29461:
	Dsp subclass: #IdentityDsp
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Unordered'!
	*/
	/*
	udanax-top.st:29465:
	IdentityDsp comment:
	'An implementation sharing convenience for Dsp classes which only provide the identity mapping functionality for their coordinate spaces.  This provides everything except the coordinate space itself (which must be provided by the subclass).  Will eventually be declared NOT_A_TYPE, so don''t use it in type declarations.  
		
		Assumes that if a given space uses it as its identity Dsp, then the one cached instance will be the only identity Dsp for that space.  I.e., I do equality comparison as an EQ object.  If this assumpsion isn''t true, please override isEqual and hashForEqual.  See PathDsp.
		
		IdentityDsp is in module "unorder" because typically unordered spaces will only have an identity Dsp and so want to subclass this class.  Non-unordered spaces should also feel free to use this as appropriate.'!
	*/
	/*
	udanax-top.st:29471:
	(IdentityDsp getOrMakeCxxClassDescription)
		friends:
	'/- friends for class IdentityDsp -/
	friend SPTR(Dsp) dsp(CoordinateSpace*);
	friend SPTR(Dsp) dsp(IntegerVar);';
		attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:29545:
	IdentityDsp class
		instanceVariableNames: 'theDsp {IdentityDsp star} '!
	*/
	/*
	udanax-top.st:29548:
	(IdentityDsp getOrMakeCxxClassDescription)
		friends:
	'/- friends for class IdentityDsp -/
	friend SPTR(Dsp) dsp(CoordinateSpace*);
	friend SPTR(Dsp) dsp(IntegerVar);';
		attributes: ((Set new) add: #NOT.A.TYPE; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected IdentityDsp() {
		super();
		/*
		udanax-top.st:29480:IdentityDsp methodsFor: 'creation'!
		create
			super create!
		*/
	}

	protected IdentityDsp(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Transforming

	public Position inverseOf(Position pos) {
		return pos;
		/*
		udanax-top.st:29485:IdentityDsp methodsFor: 'transforming'!
		{Position} inverseOf: pos {Position}
			^pos!
		*/
	}

	public XnRegion inverseOfAll(XnRegion reg) {
		return reg;
		/*
		udanax-top.st:29489:IdentityDsp methodsFor: 'transforming'!
		{XnRegion} inverseOfAll: reg {XnRegion}
			^reg!
		*/
	}

	public Position of(Position pos) {
		return pos;
		/*
		udanax-top.st:29493:IdentityDsp methodsFor: 'transforming'!
		{Position} of: pos {Position}
			^pos!
		*/
	}

	public XnRegion ofAll(XnRegion reg) {
		return reg;
		/*
		udanax-top.st:29497:IdentityDsp methodsFor: 'transforming'!
		{XnRegion} ofAll: reg {XnRegion}
			^reg!
		*/
	}

	public Dsp compose(Dsp other) {
		return other;
		/*
		udanax-top.st:29503:IdentityDsp methodsFor: 'combining'!
		{Dsp} compose: other {Dsp}
			^ other!
		*/
	}

	public Mapping inverse() {
		return this;
		/*
		udanax-top.st:29507:IdentityDsp methodsFor: 'combining'!
		{Mapping} inverse
			^ self!
		*/
	}

	public Dsp inverseCompose(Dsp other) {
		return other;
		/*
		udanax-top.st:29511:IdentityDsp methodsFor: 'combining'!
		{Dsp} inverseCompose: other {Dsp}
			^ other!
		*/
	}

	public Dsp minus(Dsp other) {
		return (Dsp) other.inverse();
		/*
		udanax-top.st:29515:IdentityDsp methodsFor: 'combining'!
		{Dsp} minus: other {Dsp}
			^other inverse cast: Dsp!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(coordinateSpace());
		oo.print(")");
		/*
		udanax-top.st:29521:IdentityDsp methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << self coordinateSpace << ')'!
		*/
	}

	public boolean isIdentity() {
		return true;
		/*
		udanax-top.st:29527:IdentityDsp methodsFor: 'accessing'!
		{BooleanVar} isIdentity
			^ true!
		*/
	}

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:29532:IdentityDsp methodsFor: 'deferred accessing'!
	{CoordinateSpace} coordinateSpace
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		TODOreturn Heaper.takeOop();
		/*
		udanax-top.st:29538:IdentityDsp methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	public boolean isEqual(Heaper other) {
		return this == other;
		/*
		udanax-top.st:29541:IdentityDsp methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			^self == other!
		*/
	}

//	public static void initTimeInherited() {
//		theDsp = new IdentityDsp();
//		//theDsp = (new(PERSISTENT)) new IdentityDsp();
//		/*
//		udanax-top.st:29557:IdentityDsp class methodsFor: 'smalltalk: smalltalk initialization'!
//		initTimeInherited
//			theDsp _ (self new.AllocType: #PERSISTENT) create.!
//		*/
//	}

	//	public static void linkTimeInherited() {
	//		theDsp = null;
	//		/*
	//		udanax-top.st:29561:IdentityDsp class methodsFor: 'smalltalk: smalltalk initialization'!
	//		linkTimeInherited
	//			theDsp _ NULL.!
	//		*/
	//	}

	//	public static void suppressInitTimeInherited() {
	//		/*
	//		udanax-top.st:29565:IdentityDsp class methodsFor: 'smalltalk: smalltalk initialization'!
	//		suppressInitTimeInherited!
	//		*/
	//	}

	//	public static void suppressLinkTimeInherited() {
	//		/*
	//		udanax-top.st:29567:IdentityDsp class methodsFor: 'smalltalk: smalltalk initialization'!
	//		suppressLinkTimeInherited!
	//		*/
	//	}
}
