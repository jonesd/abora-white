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
package info.dgjones.abora.white.stepper;

import info.dgjones.abora.white.collection.steppers.Stepper;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * This is a Stepper when you just want to step across a single item.
 */
public class EmptyStepper extends Stepper {
	/*
	udanax-top.st:53740:
	Stepper subclass: #EmptyStepper
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-stepper'!
	*/
	/*
	udanax-top.st:53744:
	EmptyStepper comment:
	'This is a Stepper when you just want to step across a single item.'!
	*/
	/*
	udanax-top.st:53746:
	(EmptyStepper getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; yourself)!
	*/

	/**
	 * This object is a canonical single instance, so its destructor should only be called after
	 * main has exited.
	 */
	public void destruct() {
		/* translateOnly "if (!!Initializer::inStaticDestruction()) BLAST(SanityViolation);" */
		super.destruct();
		/*
		udanax-top.st:53751:EmptyStepper methodsFor: 'protected: destruct'!
		{void} destruct
			"This object is a canonical single instance, so its destructor should only be called after main has exited."
			'if (!!Initializer::inStaticDestruction()) BLAST(SanityViolation);' translateOnly.
			super destruct!
		*/
	}

	public Stepper copy() {
		return this;
		/*
		udanax-top.st:53759:EmptyStepper methodsFor: 'create'!
		{Stepper} copy
			^ self!
		*/
	}

	/**
	 * No
	 */
	public void destroy() {
		/*
		udanax-top.st:53762:EmptyStepper methodsFor: 'create'!
		{void} destroy
			"No"!
		*/
	}

	public Heaper fetch() {
		return null;
		/*
		udanax-top.st:53767:EmptyStepper methodsFor: 'operations'!
		{Heaper wimpy} fetch
			^ NULL!
		*/
	}

	public boolean hasValue() {
		return false;
		/*
		udanax-top.st:53770:EmptyStepper methodsFor: 'operations'!
		{BooleanVar} hasValue
			^ false!
		*/
	}

	public void step() {
		/*
		udanax-top.st:53773:EmptyStepper methodsFor: 'operations'!
		{void} step!
		*/
	}
}
