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

import info.dgjones.abora.white.hspace.HeaperRegion;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.UnOrdered;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * A position in a HeaperSpace that represents the identity of some particular Heaper.  See
 * class comment in HeaperSpace.
 */
public abstract class HeaperAsPosition extends UnOrdered {
	/*
	udanax-top.st:32984:
	UnOrdered subclass: #HeaperAsPosition
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Unordered'!
	*/
	/*
	udanax-top.st:32988:
	HeaperAsPosition comment:
	'A position in a HeaperSpace that represents the identity of some particular Heaper.  See class comment in HeaperSpace.'!
	*/
	/*
	udanax-top.st:32990:
	(HeaperAsPosition getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:33023:
	HeaperAsPosition class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:33026:
	(HeaperAsPosition getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected HeaperAsPosition() {
		super();
	}

	protected HeaperAsPosition(Rcvr rcvr) {
		super(rcvr);
	}

	public int actualHashForEqual() {
		return System.identityHashCode(this);
		//		TODOreturn Heaper.takeOop();
		/*
		udanax-top.st:32995:HeaperAsPosition methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^Heaper takeOop!
		*/
	}

	public abstract boolean isEqual(Heaper other);
	/*
	udanax-top.st:32999:HeaperAsPosition methodsFor: 'testing'!
	{BooleanVar} isEqual: other {Heaper} 
		self subclassResponsibility!
	*/

	public XnRegion asRegion() {
		return HeaperRegion.make(this);
		/*
		udanax-top.st:33004:HeaperAsPosition methodsFor: 'accessing'!
		{XnRegion INLINE} asRegion
			^HeaperRegion make.HeaperAsPosition: self!
		*/
	}

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:33008:HeaperAsPosition methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		self subclassResponsibility!
	*/

	/**
	 * Return the underlying Heaper whose identity (as a position) I
	 * represent.
	 * It is considered good form not to use this message. There is some
	 * controversy as to whether it will go away in the future. If you
	 * know of any good reason why it should stick around please let us
	 * know.
	 */
	public abstract Heaper heaper();
	/*
	udanax-top.st:33011:HeaperAsPosition methodsFor: 'accessing'!
	{Heaper} heaper
		"Return the underlying Heaper whose identity (as a position) I 
		represent. 
		
		It is considered good form not to use this message. There is some 
		controversy as to whether it will go away in the future. If you 
		know of any good reason why it should stick around please let us 
		know."
		self subclassResponsibility!
	*/

	/**
	 * Return a HeaperAsPosition which represents the identity of this Heaper.  The resulting
	 * HeaperAsPosition will strongly retain the original Heaper against garbage collection
	 * (though not of course against manual deletion).  See wimpyAsPosition
	 */
	public static HeaperAsPosition make(Heaper heaper) {
		return new StrongAsPosition(heaper);
		/*
		udanax-top.st:33031:HeaperAsPosition class methodsFor: 'pseudo constructors'!
		{HeaperAsPosition} make: heaper {Heaper}
			"Return a HeaperAsPosition which represents the identity of this Heaper.  The resulting HeaperAsPosition will strongly retain the original Heaper against garbage collection (though not of course against manual deletion).  See wimpyAsPosition"
			
			^StrongAsPosition create: heaper!
		*/
	}
}
