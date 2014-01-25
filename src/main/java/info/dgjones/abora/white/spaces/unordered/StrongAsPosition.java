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
package info.dgjones.abora.white.spaces.unordered;

import java.io.PrintWriter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.xpp.basic.Heaper;

public class StrongAsPosition extends HeaperAsPosition {
	protected Heaper itsHeaper;
	/*
	udanax-top.st:33036:
	HeaperAsPosition subclass: #StrongAsPosition
		instanceVariableNames: 'itsHeaper {Heaper}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Unordered'!
	*/
	/*
	udanax-top.st:33040:
	(StrongAsPosition getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	public int actualHashForEqual() {
		return itsHeaper.hashForEqual();
		/*
		udanax-top.st:33045:StrongAsPosition methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^itsHeaper hashForEqual!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof HeaperAsPosition) {
			HeaperAsPosition hap = (HeaperAsPosition) other;
			return itsHeaper == hap.heaper() || (itsHeaper.isEqual(hap.heaper()));
		} else {
			return false;
		}
		/*
		udanax-top.st:33048:StrongAsPosition methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: HeaperAsPosition into: [:hap | 
					^itsHeaper == hap heaper 
						or: [itsHeaper isEqual: hap heaper]]
				others: [^false].
			^false "fodder"!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return HeaperSpace.make();
		/*
		udanax-top.st:33058:StrongAsPosition methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^ HeaperSpace make!
		*/
	}

	public Heaper heaper() {
		return itsHeaper;
		/*
		udanax-top.st:33061:StrongAsPosition methodsFor: 'accessing'!
		{Heaper} heaper
			^ itsHeaper!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print("position of (");
		oo.print(itsHeaper);
		oo.print(")");
		/*
		udanax-top.st:33066:StrongAsPosition methodsFor: 'printing'!
		{void} printOn: oo {ostream reference} 
			oo << 'position of (' << itsHeaper << ')'!
		*/
	}

	public StrongAsPosition(Heaper aHeaper) {
		super();
		if (aHeaper != null) {
			throw new IllegalArgumentException("Heapers in StrongAsPosition must be real");
		}
		itsHeaper = aHeaper;
		/*
		udanax-top.st:33071:StrongAsPosition methodsFor: 'instance creation'!
		create: aHeaper {Heaper}
			super create.
			aHeaper ~~ NULL assert: 'Heapers in StrongAsPosition must be real'.
			itsHeaper _ aHeaper!
		*/
	}

	public StrongAsPosition(Rcvr receiver) {
		super(receiver);
		itsHeaper = receiver.receiveHeaper();
		/*
		udanax-top.st:33078:StrongAsPosition methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			itsHeaper _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(itsHeaper);
		/*
		udanax-top.st:33082:StrongAsPosition methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: itsHeaper.!
		*/
	}
}
