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
package org.abora.white.spaces.unordered;

import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.SpecialistRcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.Dsp;
import org.abora.white.xpp.basic.Heaper;

public class HeaperDsp extends IdentityDsp {
	private static IdentityDsp theDsp = new HeaperDsp();

	/*
	udanax-top.st:29638:
	IdentityDsp subclass: #HeaperDsp
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Unordered'!
	*/
	/*
	udanax-top.st:29642:
	(HeaperDsp getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:29661:
	HeaperDsp class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:29664:
	(HeaperDsp getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #PSEUDO.COPY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	public CoordinateSpace coordinateSpace() {
		return HeaperSpace.make();
		/*
		udanax-top.st:29647:HeaperDsp methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^HeaperSpace make!
		*/
	}

	public HeaperDsp() {
		super();
		/*
		udanax-top.st:29653:HeaperDsp methodsFor: 'creation'!
		create
			super create!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		/*
		udanax-top.st:29658:HeaperDsp methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}!
		*/
	}

	public static Dsp make() {
		return theDsp;
		/*
		udanax-top.st:29669:HeaperDsp class methodsFor: 'pseudo constructors'!
		{Dsp} make 
			^(theDsp basicCast: IdentityDsp) basicCast: HeaperDsp!
		*/
	}

	public static Heaper make(Rcvr rcvr) {
		((SpecialistRcvr) rcvr).registerIbid(theDsp);
		return theDsp;
		/*
		udanax-top.st:29672:HeaperDsp class methodsFor: 'pseudo constructors'!
		{Heaper} make.Rcvr: rcvr {Rcvr}
			(rcvr cast: SpecialistRcvr) registerIbid: theDsp.
			^theDsp!
		*/
	}
}
