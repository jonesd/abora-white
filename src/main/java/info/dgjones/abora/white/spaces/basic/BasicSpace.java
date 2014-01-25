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

import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * BasicSpace versus CoordinateSpace is not a type distinction in that there is no difference
 * in contract with the client.  BasicSpace exists as a convenience to the definer of new
 * CoordinateSpaces.  A new subclass of CoordinateSpace should be a subclass of BasicSpace
 * iff there is only one coordinateSpace that corresponds to the new class.  I.e., that the
 * instances are not parameterized to yield different coordinate spaces.  BasicSpace provides
 * some conveniences (especially in Smalltalk) for defining a single canonical instance at
 * dynamic initialization time, and always using it.
 * As this class is irrelevent to CoordinateSpace clients, but is useful to those defining
 * other kinds of coordinate spaces, it is an exellent example of something that would be
 * classified as a "protected" class--something to be persued if we try to make modules more
 * like classes.
 */
public class BasicSpace extends CoordinateSpace {
	// Class Instance Variable
	private static BasicSpace theSpace = new BasicSpace();
	/*
	udanax-top.st:14503:
	CoordinateSpace subclass: #BasicSpace
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Spaces-Basic'!
	*/
	/*
	udanax-top.st:14507:
	BasicSpace comment:
	'BasicSpace versus CoordinateSpace is not a type distinction in that there is no difference in contract with the client.  BasicSpace exists as a convenience to the definer of new CoordinateSpaces.  A new subclass of CoordinateSpace should be a subclass of BasicSpace iff there is only one coordinateSpace that corresponds to the new class.  I.e., that the instances are not parameterized to yield different coordinate spaces.  BasicSpace provides some conveniences (especially in Smalltalk) for defining a single canonical instance at dynamic initialization time, and always using it.
		
		As this class is irrelevent to CoordinateSpace clients, but is useful to those defining other kinds of coordinate spaces, it is an exellent example of something that would be classified as a "protected" class--something to be persued if we try to make modules more like classes.'!
	*/
	/*
	udanax-top.st:14511:
	(BasicSpace getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #OBSOLETE; add: #SMALLTALK.ONLY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/
	/*
	udanax-top.st:14552:
	BasicSpace class
		instanceVariableNames: 'theSpace {BasicSpace star} '!
	*/
	/*
	udanax-top.st:14555:
	(BasicSpace getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #OBSOLETE; add: #SMALLTALK.ONLY; add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public int actualHashForEqual() {
		return getClass().hashCode() + 1;
		//TODOreturn getCategory().hashForEqual() + 1;
		/*
		udanax-top.st:14516:BasicSpace methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			"is equal to any basic space on the same category of positions"
			^self getCategory hashForEqual + 1!
		*/
	}

	/**
	 * is equal to any basic space on the same category of positions
	 */
	public boolean isEqual(Heaper anObject) {
		return anObject.getClass() == getClass();
		/*
		udanax-top.st:14521:BasicSpace methodsFor: 'testing'!
		{BooleanVar} isEqual: anObject {Heaper}
			"is equal to any basic space on the same category of positions"
			^anObject getCategory == self getCategory!
		*/
	}

	protected BasicSpace() {
		super();
	}

	public BasicSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending, OrderSpec descending) {
		super(emptyRegion, fullRegion, identityDsp, ascending, descending);
		/*
		udanax-top.st:14528:BasicSpace methodsFor: 'creation'!
		create: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			with: ascending {OrderSpec default: NULL}
			with: descending {OrderSpec default: NULL}
			
			super create: emptyRegion with: fullRegion with: identityDsp with: ascending with: descending.!
		*/
	}

	public BasicSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp) {
		new BasicSpace(emptyRegion, fullRegion, identityDsp, null, null);
		/*
		udanax-top.st:14538:BasicSpace methodsFor: 'smalltalk: defaults'!
		create: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			
			self create: emptyRegion with: fullRegion with: identityDsp with: NULL with: NULL!
		*/
	}

	public BasicSpace(XnRegion emptyRegion, XnRegion fullRegion, Dsp identityDsp, OrderSpec ascending) {
		new BasicSpace(emptyRegion, fullRegion, identityDsp, ascending, null);
		/*
		udanax-top.st:14544:BasicSpace methodsFor: 'smalltalk: defaults'!
		create: emptyRegion {XnRegion}
			with: fullRegion {XnRegion}
			with: identityDsp {Dsp}
			with: ascending {OrderSpec default: NULL}
			
			self create: emptyRegion with: fullRegion with: identityDsp with: ascending with: NULL!
		*/
	}

//	public static void initTimeInherited() {
////		REQUIRES(PrimSpec.getCategory());
//		theSpace = new BasicSpace();
//		/*
//		udanax-top.st:14560:BasicSpace class methodsFor: 'smalltalk: initialization'!
//		initTimeInherited
//			self REQUIRES: PrimSpec.
//			theSpace _ (self new.AllocType: #PERSISTENT) create.!
//		*/
//	}

//	public static void linkTimeInherited() {
//		theSpace = null;
//		/*
//		udanax-top.st:14565:BasicSpace class methodsFor: 'smalltalk: initialization'!
//		linkTimeInherited
//			theSpace _ NULL.!
//		*/
//	}

//	public static void suppressInitTimeInherited() {
//		/*
//		udanax-top.st:14569:BasicSpace class methodsFor: 'smalltalk: initialization'!
//		suppressInitTimeInherited!
//		*/
//	}

//	public static void suppressLinkTimeInherited() {
//		/*
//		udanax-top.st:14571:BasicSpace class methodsFor: 'smalltalk: initialization'!
//		suppressLinkTimeInherited!
//		*/
//	}
}
