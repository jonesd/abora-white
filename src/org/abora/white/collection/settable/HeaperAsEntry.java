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
package org.abora.white.collection.settable;

import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.unordered.HeaperAsPosition;
import org.abora.white.xpp.basic.Heaper;

public class HeaperAsEntry extends TableEntry {
	/*
	udanax-top.st:56639:
	TableEntry subclass: #HeaperAsEntry
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-SetTable'!
	*/
	/*
	udanax-top.st:56643:
	(HeaperAsEntry getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/**
	 * Return true if my position matches position.
	 */
	public boolean match(Position position) {
		return position().isEqual(position);
		/*
		udanax-top.st:56648:HeaperAsEntry methodsFor: 'accessing'!
		{BooleanVar} match: position {Position}
			"Return true if my position matches position."
			
			^self position isEqual: position!
		*/
	}

	public Position position() {
		return HeaperAsPosition.make(value());
		/*
		udanax-top.st:56653:HeaperAsEntry methodsFor: 'accessing'!
		{Position} position
			^HeaperAsPosition make: self value!
		*/
	}

	/**
	 * Return true if my value can be replaced in place, and false if the entire entry must be
	 * replaced.
	 */
	public boolean replaceValue(Heaper newValue) {
		return newValue.hashForEqual() == value().hashForEqual() && (super.replaceValue(newValue));
		/*
		udanax-top.st:56656:HeaperAsEntry methodsFor: 'accessing'!
		{BooleanVar} replaceValue: newValue {Heaper}
			"Return true if my value can be replaced in place, and false if the entire entry must be replaced."
			
			^newValue hashForEqual == self value hashForEqual and: [super replaceValue: newValue]!
		*/
	}

	public TableEntry copy() {
		return new HeaperAsEntry(value());
		/*
		udanax-top.st:56663:HeaperAsEntry methodsFor: 'creation'!
		{TableEntry} copy
			^ HeaperAsEntry create: self value!
		*/
	}

	protected HeaperAsEntry(Heaper value) {
		super(value);
		/*
		udanax-top.st:56666:HeaperAsEntry methodsFor: 'creation'!
		create: value {Heaper}
			super create: value!
		*/
	}

	protected HeaperAsEntry(TableEntry next, Heaper value) {
		super(next, value);
		/*
		udanax-top.st:56669:HeaperAsEntry methodsFor: 'creation'!
		create: next {TableEntry} with: value {Heaper}
			super create: next with: value!
		*/
	}

	protected HeaperAsEntry(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:56674:HeaperAsEntry methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:56677:HeaperAsEntry methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}
}
