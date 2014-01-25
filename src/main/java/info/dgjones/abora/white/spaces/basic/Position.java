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
package info.dgjones.abora.white.spaces.basic;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * This is the superclass of all positions of coordinate spaces.  Each individual position is
 * specific to some one coordinate space.  Positions themselves don''t have much behavior, as
 * most of the interesting aspects of coordinate spaces are defined in the other objects in
 * terms of positions.  Positions do have their own native ordering messages, but for most
 * purposes it''s probably better to compare them using an appropriate OrderSpec.
 */
public abstract class Position extends Heaper {
	/*
	udanax-top.st:31468:
	Heaper subclass: #Position
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:31472:
	Position comment:
	'This is the superclass of all positions of coordinate spaces.  Each individual position is specific to some one coordinate space.  Positions themselves don''t have much behavior, as most of the interesting aspects of coordinate spaces are defined in the other objects in terms of positions.  Positions do have their own native ordering messages, but for most purposes it''s probably better to compare them using an appropriate OrderSpec.'!
	*/
	/*
	udanax-top.st:31474:
	(Position getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:31516:
	Position class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:31519:
	(Position getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected Position() {
		super();
	}

	protected Position(Rcvr rcvr) {
		super(rcvr);
	}

	/**
	 * since we redefine equal, subclasses had better redefine actualHashForEqual
	 */
	public int actualHashForEqual() {

		return System.identityHashCode(this);
		//TODO return Heaper.takeOop();
		/*
		udanax-top.st:31479:Position methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			"since we redefine equal, subclasses had better redefine actualHashForEqual"
			^Heaper takeOop!
		*/
	}

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:31484:Position methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper} 
		self subclassResponsibility!
	*/

	/**
	 * Essential.  A region containing this position as its only element.
	 */
	public abstract XnRegion asRegion();
	/*
	udanax-top.st:31490:Position methodsFor: 'accessing'!
	{XnRegion CLIENT} asRegion
		"Essential.  A region containing this position as its only element."
		self subclassResponsibility!
	*/

	/**
	 * Essential.  The coordinate space this is a position in. This implies that a position
	 * object is only a position in one particular coordinate space.
	 */
	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:31494:Position methodsFor: 'accessing'!
	{CoordinateSpace CLIENT} coordinateSpace
		"Essential.  The coordinate space this is a position in. This implies that a position object is only a position in one particular coordinate space."
		self subclassResponsibility!
	*/

	///**
	// * OBSOLETE. Use OrderSpec instead, or non-polymorphic subclass methods.
	// * This must define a full ordering on all positions in the same coordinate space.
	// * As this isn''t possible for some coordinate spaces (e.g. HeaperSpace & FilterSpace), we
	// * may BLAST instead.  Therefore this message should eventually get retired -- don't use.
	// * See OrderSpec::follows for the properties a partial order must satisfy.  A full ordering
	// * must additionally satisfy: for all a, b; either a->isAfterOrEqual(b) or
	// * b->isAfterOrEqual(a).
	// */
	//public boolean isAfterOrEqual(Position other) {
	//passe();
	///*
	//udanax-top.st:31501:Position methodsFor: 'smalltalk: passe'!
	//{BooleanVar} isAfterOrEqual: other {Position}
	//	"OBSOLETE. Use OrderSpec instead, or non-polymorphic subclass methods.
	//	This must define a full ordering on all positions in the same coordinate space.
	//	As this isn''t possible for some coordinate spaces (e.g. HeaperSpace & FilterSpace), we may BLAST instead.  Therefore this message should eventually get retired -- don't use.
	//	See OrderSpec::follows for the properties a partial order must satisfy.  A full ordering must additionally satisfy: for all a, b; either a->isAfterOrEqual(b) or b->isAfterOrEqual(a)."
	//	self passe!
	//*/
	//}

	///**
	// * OBSOLETE. Use the OrderSpec, or non-polymorphic subclass methods.
	// * Defines a transitive partial order; return false if incompatible.  See OrderSpec::follows
	// * for the properties a partial order must satisfy.  The ordering according to isGE is the
	// * same as the ascending OrderSpec for this coordinate space.  It is probably better to use
	// * the OrderSpec than this message.
	// */
	//public boolean isGE(Position other) {
	//passe();
	///*
	//udanax-top.st:31509:Position methodsFor: 'smalltalk: passe'!
	//{BooleanVar} isGE: other {Position}
	//	"OBSOLETE. Use the OrderSpec, or non-polymorphic subclass methods.
	//	Defines a transitive partial order; return false if incompatible.  See OrderSpec::follows for the properties a partial order must satisfy.  The ordering according to isGE is the same as the ascending OrderSpec for this coordinate space.  It is probably better to use the OrderSpec than this message."
	//	self passe!
	//*/
	//}

	/**
	 * {XuRegion CLIENT} asRegion
	 * {CoordinateSpace CLIENT} coordinateSpace
	 */
	public static void info() {
		/*
		udanax-top.st:31524:Position class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{XuRegion CLIENT} asRegion
		{CoordinateSpace CLIENT} coordinateSpace
		"!
		*/
	}
}
