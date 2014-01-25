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
package info.dgjones.abora.white.collection.settable;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class PositionEntry extends TableEntry {
	protected Position myKey;
	/*
	udanax-top.st:56736:
	TableEntry subclass: #PositionEntry
		instanceVariableNames: 'myKey {Position}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-SetTable'!
	*/
	/*
	udanax-top.st:56740:
	(PositionEntry getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	/**
	 * Return true if my key matches key.
	 */
	public boolean match(Position key) {
		return key.isEqual(myKey);
		/*
		udanax-top.st:56745:PositionEntry methodsFor: 'accessing'!
		{BooleanVar} match: key {Position}
			"Return true if my key matches key."
			
			^key isEqual: myKey!
		*/
	}

	public Position position() {
		return myKey;
		/*
		udanax-top.st:56750:PositionEntry methodsFor: 'accessing'!
		{Position} position
			^myKey!
		*/
	}

	public TableEntry copy() {
		return new PositionEntry(myKey, value());
		/*
		udanax-top.st:56755:PositionEntry methodsFor: 'creation'!
		{TableEntry} copy
			^ PositionEntry create: myKey with: self value!
		*/
	}

	public PositionEntry(Position key, Heaper value) {
		super(value);
		myKey = key;
		/*
		udanax-top.st:56758:PositionEntry methodsFor: 'creation'!
		create: key {Position} with: value {Heaper}
			super create: value.
			myKey _ key!
		*/
	}

	public PositionEntry(TableEntry next, Heaper value, Position key) {
		super(next, value);
		myKey = key;
		/*
		udanax-top.st:56762:PositionEntry methodsFor: 'creation'!
		create: next {TableEntry} with: value {Heaper} with: key {Position}
			super create: next with: value.
			myKey _ key!
		*/
	}

	public PositionEntry(Rcvr receiver) {
		super(receiver);
		myKey = (Position) receiver.receiveHeaper();
		/*
		udanax-top.st:56768:PositionEntry methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myKey _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myKey);
		/*
		udanax-top.st:56772:PositionEntry methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myKey.!
		*/
	}
}
