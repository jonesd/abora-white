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
package info.dgjones.abora.white.collection.tables;

import java.io.PrintWriter;

import info.dgjones.abora.white.exception.AboraRuntimeException;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * Sometimes you just want to pass around two things where the language only makes it
 * convenient to pass around one.  I know that the proper object-oriented (or even
 * "structured") thing to do would be to create a type specific to the particular kind of
 * pair which is being used for a particular purpose.  However, sometimes it just seems like
 * too much trouble.  By using Pairs, we import the sins of Lisp.  At least we don''t have
 * RPLACA and RPLACD.  Unlike Lisp's cons cell's "car" and "cdr", we call our two parts the
 * "left" part and the "right" part.  "<code>pair(a,b)->left()</code>" yields a and "<code>pair(a,b)->right()</code>"
 * yields b.
 * <p>
 * Give us feedback: Should Pairs be removed?  Do you know of any justification for them
 * other than a bad simulation of "multiple-return-values" (as in Common Lisp, Forth,
 * Postscript)?
 * <p>
 * The Pair code is currently in a state of transition.  Old code (which we have yet to fix)
 * uses Pairs with NULLs in their parts.  Pairs will be changed to outlaw this usage.
 * "fetchLeft" and "fetchRight" exist to support this obsolete usage, but will be retired.
 * Don't use them.
 */
public class Pair extends Heaper {
	protected Heaper leftPart;
	protected Heaper rightPart;
	/*
	udanax-top.st:31189:
	Heaper subclass: #Pair
		instanceVariableNames: '
			leftPart {Heaper copy}
			rightPart {Heaper copy}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:31195:
	Pair comment:
	'Sometimes you just want to pass around two things where the language only makes it convenient to pass around one.  I know that the proper object-oriented (or even "structured") thing to do would be to create a type specific to the particular kind of pair which is being used for a particular purpose.  However, sometimes it just seems like too much trouble.  By using Pairs, we import the sins of Lisp.  At least we don''t have RPLACA and RPLACD.  Unlike Lisp''s cons cell''s "car" and "cdr", we call our two parts the "left" part and the "right" part.  "pair(a,b)->left()" yields a and "pair(a,b)->right()" yields b.
		
		Give us feedback: Should Pairs be removed?  Do you know of any justification for them other than a bad simulation of "multiple-return-values" (as in Common Lisp, Forth, Postscript)?
		
		The Pair code is currently in a state of transition.  Old code (which we have yet to fix) uses Pairs with NULLs in their parts.  Pairs will be changed to outlaw this usage.  "fetchLeft" and "fetchRight" exist to support this obsolete usage, but will be retired.  Don''t use them.'!
	*/
	/*
	udanax-top.st:31201:
	(Pair getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:31307:
	Pair class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:31310:
	(Pair getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #COPY; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors
	
	/**
	 * create a new pair
	 */
	protected Pair(Heaper a, Heaper b) {
		super();
		leftPart = a;
		rightPart = b;
		/*
		udanax-top.st:31257:Pair methodsFor: 'instance creation'!
		create: a {Heaper} with: b {Heaper}
			"create a new pair"
			super create.
			leftPart _ a.
			rightPart _ b.!
		*/
	}

	protected Pair(Rcvr receiver) {
		super(receiver);
		leftPart = receiver.receiveHeaper();
		rightPart = receiver.receiveHeaper();
		/*
		udanax-top.st:31296:Pair methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			leftPart _ receiver receiveHeaper.
			rightPart _ receiver receiveHeaper.!
		*/
	}

	/////////////////////////////////////////////
	// Static Factory Methods

	/**
	 * Create a new pair. Since it used to be normal to allow either left or right to be
	 * NULL (it is now obsolete but supported for the moment), and it is impossible to
	 * do a static check, this (normal) pseudo-constructor does a dynamic check. If
	 * you encounter this error, the quick fix is use the obsolete pseudo-constructor
	 * (pairWithNulls). The better fix is to stop using NULLs.
	 */
	public static Pair make(Heaper left, Heaper right) {
		if (left == null || right == null) {
			throw new IllegalArgumentException();
		}
		return new Pair(left, right);
		/*
		udanax-top.st:31315:Pair class methodsFor: 'instance creation'!
		make: left {Heaper} with: right {Heaper} 
				"Create a new pair. Since it used to be normal to allow either left or right to be 
				NULL (it is now obsolete but supported for the moment), and it is impossible to 
				do a static check, this (normal) pseudo-constructor does a dynamic check. If 
				you encounter this error, the quick fix is use the obsolete pseudo-constructor 
				(pairWithNulls). The better fix is to stop using NULLs."
				(left == NULL or: [right = NULL])
						ifTrue: [Heaper BLAST: #ObsoleteUsageMustUsePairWithNulls].
				^self create: left with: right!
		*/
	}

	/**
	 * Create a new pair. Either may be NULL in order to support broken old code.
	 */
	public static Pair pairWithNulls(Heaper left, Heaper right) {
		return new Pair(left, right);
		/*
		udanax-top.st:31328:Pair class methodsFor: 'obsolete: creation'!
		{Pair} pairWithNulls: left {Heaper} with: right {Heaper} 
			"Create a new pair. Either may be NULL in order to support broken old code."
			^self create: left with: right!
		*/
	}

	/////////////////////////////////////////////
	// Accessing
	
	/**
	 * Returns the left part.  Lispers may think 'car'.
	 */
	public Heaper left() {
		if (leftPart == null) {
			throw new AboraRuntimeException(AboraRuntimeException.OBSOLETE_USAGE_MUST_USE_FETCH_LEFT);
		}
		return leftPart;
		/*
		udanax-top.st:31233:Pair methodsFor: 'accessing'!
		{Heaper} left
			"Returns the left part.  Lispers may think 'car'."
			
			leftPart == NULL
				ifTrue: [Heaper BLAST: #ObsoleteUsageMustUseFetchLeft].
			^leftPart!
		*/
	}

	/**
	 * Returns the right part.  Lispers may think 'cdr'.
	 */
	public Heaper right() {
		if (rightPart == null) {
			throw new AboraRuntimeException(AboraRuntimeException.OBSOLETE_USAGE_MUST_USE_FETCH_RIGHT);
		}
		return rightPart;
		/*
		udanax-top.st:31248:Pair methodsFor: 'accessing'!
		{Heaper} right
			"Returns the right part.  Lispers may think 'cdr'."
			
			rightPart == NULL
				ifTrue: [Heaper BLAST: #ObsoleteUsageMustUseFetchRight].
			^rightPart!
		*/
	}

	/**
	 * Returns the left part which obsoletely may be NULL
	 */
	public Heaper fetchLeft() {
		return leftPart;
		/*
		udanax-top.st:31278:Pair methodsFor: 'obsolete: access'!
		{Heaper INLINE | NULL} fetchLeft
			"Returns the left part which obsoletely may be NULL"
			
			^leftPart!
		*/
	}

	/**
	 * Returns the right part which obsoletely may be NULL
	 */
	public Heaper fetchRight() {
		return rightPart;
		/*
		udanax-top.st:31283:Pair methodsFor: 'obsolete: access'!
		{Heaper INLINE | NULL} fetchRight
			"Returns the right part which obsoletely may be NULL"
			
			^rightPart!
		*/
	}

	/**
	 * Returns a new pair which is the left-right reversal of me.
	 * pair(a,b)->reversed() is the same as pair(b,a).
	 * Only works on non-obsolete Pairs--those whose parts are non-NULL
	 */
	public Pair reversed() {
		return Pair.make(rightPart, leftPart);
		/*
		udanax-top.st:31240:Pair methodsFor: 'accessing'!
		{Pair INLINE} reversed
			"Returns a new pair which is the left-right reversal of me.
			pair(a,b)->reversed() is the same as pair(b,a).
			
			Only works on non-obsolete Pairs--those whose parts are non-NULL"
			
			^Pair make: rightPart with: leftPart!
		*/
	}

	/////////////////////////////////////////////
	// Comparing and Hashing
	
	public int actualHashForEqual() {
		int result;
		if (leftPart != null) {
			result = leftPart.hashForEqual();
		} else {
			result = 37;
		}
		if (rightPart != null) {
			return result + rightPart.hashForEqual();
		} else {
			return result + 73;
		}
		/*
		udanax-top.st:31206:Pair methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			| result {UInt32} |
			leftPart ~~ NULL
				ifTrue: [result _ leftPart hashForEqual]
				ifFalse: [result _ 37].
			rightPart ~~ NULL
				ifTrue: [^ result + rightPart hashForEqual]
				ifFalse: [^ result + 73]!
		*/
	}

	public boolean isEqual(Heaper other) {
		boolean res;
		if (other instanceof Pair) {
			Pair pair = (Pair) other;
			if (leftPart == null) {
				res = pair.fetchLeft() == null;
			} else {
				res = leftPart.isEqual(pair.fetchLeft());
			}
			if (res) {
				if (rightPart == null) {
					return pair.fetchRight() == null;
				} else {
					return rightPart.isEqual(pair.fetchRight());
				}
			} else {
				return false;
			}
		} else {
			return false;
		}
		/*
		udanax-top.st:31215:Pair methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper} 
			| res {BooleanVar} |
			other 
				cast: Pair into: [:pair |
					leftPart == NULL
						ifTrue: [res _ pair fetchLeft == NULL]
						ifFalse: [res _ leftPart isEqual: pair left].
					res 
						ifTrue: [rightPart == NULL
							ifTrue: [^pair fetchRight == NULL]
							ifFalse: [^rightPart isEqual: pair right]]
						ifFalse: [^false]]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

//	/**
//	 * Return pieces to be used in a tree browser.
//	 */
//	public void inspectPieces() {
//		return OrderedCollection.with(leftPart, rightPart);
//		/*
//		udanax-top.st:31265:Pair methodsFor: 'smalltalk:'!
//		inspectPieces
//			"Return pieces to be used in a tree browser."
//			^OrderedCollection with: leftPart with: rightPart!
//		*/
//	}

	/////////////////////////////////////////////
	// Printing

	public void printOn(PrintWriter oo) {
		oo.print("<");
		oo.print(leftPart);
		oo.print(" , ");
		oo.print(rightPart);
		oo.print(">");
		/*
		udanax-top.st:31272:Pair methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << '<' << leftPart << ' , ' << rightPart << '>'!
		*/
	}


//	/**
//	 * create a new pair
//	 */
//	public Pair(Heaper a) {
//		passe();
//		/*
//		udanax-top.st:31290:Pair methodsFor: 'smalltalk: passe'!
//		create: a {Heaper}
//			"create a new pair"
//			self passe!
//		*/
//	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(leftPart);
		xmtr.sendHeaper(rightPart);
		/*
		udanax-top.st:31301:Pair methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: leftPart.
			xmtr sendHeaper: rightPart.!
		*/
	}
}
