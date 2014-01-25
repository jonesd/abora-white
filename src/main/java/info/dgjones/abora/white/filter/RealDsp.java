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
package info.dgjones.abora.white.filter;

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.unordered.IdentityDsp;
import info.dgjones.abora.white.tumbler.RealSpace;

public class RealDsp extends IdentityDsp {
	/*
	udanax-top.st:29730:
	IdentityDsp subclass: #RealDsp
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Filter'!
	*/
	/*
	udanax-top.st:29734:
	(RealDsp getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:29752:
	RealDsp class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:29755:
	(RealDsp getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; add: #NOT.A.TYPE; yourself)!
	*/

	public CoordinateSpace coordinateSpace() {
		return RealSpace.make();
		/*
		udanax-top.st:29739:RealDsp methodsFor: 'deferred accessing'!
		{CoordinateSpace} coordinateSpace
			^RealSpace make!
		*/
	}

	protected RealDsp() {
		super();
	}

	protected RealDsp(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:29745:RealDsp methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:29748:RealDsp methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static RealDsp make() {
		return new RealDsp();
		/*
		udanax-top.st:29760:RealDsp class methodsFor: 'creation'!
		{Dsp} make
			
			^self create!
		*/
	}
}
