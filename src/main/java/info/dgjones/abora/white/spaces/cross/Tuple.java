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
package info.dgjones.abora.white.spaces.cross;

import java.io.PrintWriter;

import info.dgjones.abora.white.collection.arrays.PtrArray;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.PrimSpec;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A tuple is a Position in a CrossSpace represented by a sequence of Positions in its
 * subSpaces
 */
public abstract class Tuple extends Position {
	/*
	udanax-top.st:32763:
	Position subclass: #Tuple
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Cross'!
	*/
	/*
	udanax-top.st:32767:
	Tuple comment:
	'A tuple is a Position in a CrossSpace represented by a sequence of Positions in its subSpaces'!
	*/
	/*
	udanax-top.st:32769:
	(Tuple getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:32834:
	Tuple class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:32837:
	(Tuple getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #ON.CLIENT; add: #DEFERRED; add: #COPY; yourself)!
	*/

	public void printOn(PrintWriter oo) {
		printOnWithSimpleSyntax(oo, "<", ", ", ">");
		/*
		udanax-top.st:32774:Tuple methodsFor: 'printing'!
		{void} printOn: oo {ostream reference} 
			self printOnWithSimpleSyntax: oo
				with: '<'
				with: ', '
				with: '>'!
		*/
	}

	public void printOnWithSimpleSyntax(PrintWriter oo, String openString, String sep, String closeString) {
		PtrArray coords;
		oo.print(openString);
		coords = coordinates();
		for (int i = 0; i < coords.count(); i++) {
			if (i > 0) {
				oo.print(sep);
			}
			(coords.fetch(i)).printOn(oo);
		}
		oo.print(closeString);
		/*
		udanax-top.st:32781:Tuple methodsFor: 'printing'!
		{void} printOnWithSimpleSyntax: oo {ostream reference} 
			with: openString {char star} 
			with: sep {char star} 
			with: closeString {char star}
			| coords {PtrArray of: Position} |
			oo << openString.
			coords := self coordinates.
			Int32Zero almostTo: coords count do: [:i {Int32} |
				i > Int32Zero ifTrue:
					[oo << sep].
				(coords fetch: i) printOn: oo].
			oo << closeString!
		*/
	}

	public abstract XnRegion asRegion();
	/*
	udanax-top.st:32797:Tuple methodsFor: 'accessing'!
	{XnRegion} asRegion
		self subclassResponsibility!
	*/

	/**
	 * The position with in a subspace
	 */
	public Position coordinate(int index) {
		return (Position) (coordinates().fetch(index));
		/*
		udanax-top.st:32801:Tuple methodsFor: 'accessing'!
		{Position CLIENT} coordinate: index {Int32}
			"The position with in a subspace"
			
			^(self coordinates fetch: index) cast: Position!
		*/
	}

	/**
	 * Essential. An array of the coordinates in each sub space
	 */
	public abstract PtrArray coordinates();
	/*
	udanax-top.st:32806:Tuple methodsFor: 'accessing'!
	{PtrArray CLIENT of: Position} coordinates
		"Essential. An array of the coordinates in each sub space"
		
		self subclassResponsibility!
	*/

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:32811:Tuple methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//return Heaper.takeOop();
		/*
		udanax-top.st:32817:Tuple methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			
			^Heaper takeOop!
		*/
	}

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:32821:Tuple methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper}
		self subclassResponsibility!
	*/

	protected Tuple() {
		super();
	}

	protected Tuple(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:32827:Tuple methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:32830:Tuple methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static Tuple make(PtrArray coordinates) {
		return ActualTuple.make(((PtrArray) coordinates.copy()));
		/*
		udanax-top.st:32842:Tuple class methodsFor: 'pseudoconstructors'!
		make: coordinates {PtrArray of: Position}
			^ActualTuple make: (coordinates copy cast: PtrArray)!
		*/
	}

	public static Tuple two(Position zero, Position one) {
		return ActualTuple.make(((PtrArray) (PrimSpec.pointer().arrayWithTwo(zero, one))));
		/*
		udanax-top.st:32846:Tuple class methodsFor: 'pseudoconstructors'!
		{Tuple} two: zero {Position} with: one {Position}
			^ActualTuple make: ((PrimSpec pointer arrayWithTwo: zero with: one) cast: PtrArray)!
		*/
	}

	/**
	 * {Position CLIENT} coordinate: index {Int32}
	 * {PtrArray CLIENT of: Position} coordinates
	 */
	public static void info() {
		/*
		udanax-top.st:32852:Tuple class methodsFor: 'smalltalk: system'!
		info.stProtocol
		"{Position CLIENT} coordinate: index {Int32}
		{PtrArray CLIENT of: Position} coordinates
		"!
		*/
	}
}
