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

import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.SpecialistRcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.xpp.basic.Heaper;

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
