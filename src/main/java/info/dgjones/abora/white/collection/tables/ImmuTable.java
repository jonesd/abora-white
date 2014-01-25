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
package info.dgjones.abora.white.collection.tables;

import info.dgjones.abora.white.collection.steppers.TableStepper;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.Dsp;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.Position;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.value.IntegerValue;
import info.dgjones.abora.white.xpp.basic.Heaper;

/**
 * ImmuTable are to ScruTables much like ImmuSets are to ScruSets.  See ImmuSet.
 * The ImmuTable subclass of tables represents all tables which CANNOT be side-effected
 * during operations on them.  They are intended to represent mathematical abstractions (such
 * as vectors) and are intended to be used in a functional-programming style.  Operations are
 * provided for building new ImmuTables out of old ones.
 */
public abstract class ImmuTable extends ScruTable {
	/*
	udanax-top.st:47246:
	ScruTable subclass: #ImmuTable
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:47250:
	ImmuTable comment:
	'ImmuTable are to ScruTables much like ImmuSets are to ScruSets.  See ImmuSet.
		
		The ImmuTable subclass of tables represents all tables which CANNOT be side-effected during operations on them.  They are intended to represent mathematical abstractions (such as vectors) and are intended to be used in a functional-programming style.  Operations are provided for building new ImmuTables out of old ones.'!
	*/
	/*
	udanax-top.st:47254:
	(ImmuTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/
	/*
	udanax-top.st:47383:
	ImmuTable class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:47386:
	(ImmuTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #DEFERRED; yourself)!
	*/

	/////////////////////////////////////////////
	// Constructors

	protected ImmuTable() {
		super();
	}

	protected ImmuTable(Rcvr rcvr) {
		super(rcvr);
	}

	/////////////////////////////////////////////
	// Accessing

	public abstract CoordinateSpace coordinateSpace();
	/*
	udanax-top.st:47259:ImmuTable methodsFor: 'accessing'!
	{CoordinateSpace} coordinateSpace
		
		self subclassResponsibility!
	*/

	public abstract IntegerValue count();
	/*
	udanax-top.st:47263:ImmuTable methodsFor: 'accessing'!
	{IntegerVar} count
		self subclassResponsibility.!
	*/

	public abstract XnRegion domain();
	/*
	udanax-top.st:47267:ImmuTable methodsFor: 'accessing'!
	{XnRegion} domain
		self subclassResponsibility.!
	*/

	public abstract Heaper fetch(Position key);
	/*
	udanax-top.st:47271:ImmuTable methodsFor: 'accessing'!
	{Heaper} fetch: key {Position} 
		self subclassResponsibility!
	*/

	public Heaper intFetch(IntegerValue key) {
		return super.intFetch(key);
		/*
		udanax-top.st:47275:ImmuTable methodsFor: 'accessing'!
		{Heaper} intFetch: key {IntegerVar}
			^ super intFetch: key!
		*/
	}

	public abstract XnRegion runAt(Position key);
	/*
	udanax-top.st:47278:ImmuTable methodsFor: 'accessing'!
	{XnRegion} runAt: key {Position} 
		self subclassResponsibility!
	*/

	public abstract XnRegion runAtInt(IntegerValue key);
	/*
	udanax-top.st:47282:ImmuTable methodsFor: 'accessing'!
	{XnRegion} runAtInt: key {IntegerVar}
		^ super runAtInt: key!
	*/

	public abstract ScruTable subTable(XnRegion reg);
	/*
	udanax-top.st:47285:ImmuTable methodsFor: 'accessing'!
	{ScruTable} subTable: reg {XnRegion} 
		self subclassResponsibility!
	*/

	/**
	 * Return a ScruTable with the domain of the receiver transformed by the Dsp.
	 * 'table->transformedBy(d)->fetch(p)' is equivalent to
	 * 'table->fetch(d->of(p))'.
	 * See ScruTable::subTable for caveats regarding whether we return a snapshot
	 * or a view. All the same caveats apply.
	 * In this case of transforming an ImmuTable, it makes sense to return an ImmuTable.
	 */
	public ScruTable transformedBy(Dsp dsp) {
		return new OffsetImmuTable(this, dsp);
		/*
		udanax-top.st:47289:ImmuTable methodsFor: 'accessing'!
		{ScruTable} transformedBy: dsp {Dsp} 
			"Return a ScruTable with the domain of the receiver transformed by the Dsp. 
			'table->transformedBy(d)->fetch(p)' is equivalent to 
			'table->fetch(d->of(p))'. 
			
			See ScruTable::subTable for caveats regarding whether we return a snapshot 
			or a view. All the same caveats apply.
			
			In this case of transforming an ImmuTable, it makes sense to return an ImmuTable."
			^ OffsetImmuTable create: self with: dsp!
		*/
	}

	/**
	 * don't need to actually make a copy, as this is immutable
	 */
	public ScruTable copy() {
		return this;
		/*
		udanax-top.st:47303:ImmuTable methodsFor: 'creation'!
		{ScruTable} copy
			"don't need to actually make a copy, as this is immutable"
			^self!
		*/
	}

	/**
	 * The idea of a 'size' argument would seem kind of ridiculous here as the resulting empty
	 * table can't be changed.
	 */
	public abstract ScruTable emptySize(IntegerValue size);
	/*
	udanax-top.st:47307:ImmuTable methodsFor: 'creation'!
	{ScruTable} emptySize: size {IntegerVar}
		"The idea of a 'size' argument would seem kind of ridiculous here as the resulting empty table can't be changed."
		
		self subclassResponsibility.!
	*/

	/**
	 * Similar to unionWith.  In particular, if 'a = b->combineWith(c);', then:
	 * 'a->domain()->isEqual(b->domain()->unionWith(c->domain())' and
	 * 'a->range()->isSubsetOf(b->range()->unionWith(c->range())'.
	 * (Note that the domain case uses XuRegion::unionWith, while the range case
	 * uses ImmuSet::unionWith.)
	 * Despite this correspondence, unionWith is symmetrical while combineWith is not.
	 * Given that the two input tables have different associations for a given key,
	 * one gets to dominate.  I need to specify which one here, but the code seems
	 * inconsistent on this question.  Until this is resolved, console youself with the
	 * thought that if the tables don't conflict we have a simple unionWith of the two
	 * sets of associations (and the 'isSubsetOf' above can be replaced with 'isEqual').
	 */
	public abstract ImmuTable combineWith(ImmuTable other);
	/*
	udanax-top.st:47314:ImmuTable methodsFor: 'SEF manipulation'!
	{ImmuTable} combineWith: other {ImmuTable}
		"Similar to unionWith.  In particular, if 'a = b->combineWith(c);', then:
		'a->domain()->isEqual(b->domain()->unionWith(c->domain())' and
		'a->range()->isSubsetOf(b->range()->unionWith(c->range())'.
		(Note that the domain case uses XuRegion::unionWith, while the range case
		uses ImmuSet::unionWith.)
		
		Despite this correspondence, unionWith is symmetrical while combineWith is not.
		Given that the two input tables have different associations for a given key,
		one gets to dominate.  I need to specify which one here, but the code seems
		inconsistent on this question.  Until this is resolved, console youself with the
		thought that if the tables don't conflict we have a simple unionWith of the two
		sets of associations (and the 'isSubsetOf' above can be replaced with 'isEqual')."
		
		self subclassResponsibility!
	*/

	/**
	 * Return a new table just like the current one except with the association whose
	 * key is 'index'.
	 */
	public abstract ImmuTable without(Position index);
	/*
	udanax-top.st:47330:ImmuTable methodsFor: 'SEF manipulation'!
	{ImmuTable} without: index {Position} 
		"Return a new table just like the current one except with the association whose 
		key is 'index'."
		self subclassResponsibility!
	*/

	public int actualHashForEqual() {
		return getClass().hashCode() + contentsHash();
		/*
		udanax-top.st:47338:ImmuTable methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^#cat.U.ImmuTable hashForEqual + self contentsHash!
		*/
	}

	public boolean includesIntKey(IntegerValue aKey) {
		return super.includesIntKey(aKey);
		/*
		udanax-top.st:47341:ImmuTable methodsFor: 'testing'!
		{BooleanVar} includesIntKey: aKey {IntegerVar}
			^ super includesIntKey: aKey!
		*/
	}

	public abstract boolean includesKey(Position aKey);
	/*
	udanax-top.st:47344:ImmuTable methodsFor: 'testing'!
	{BooleanVar} includesKey: aKey {Position}
		self subclassResponsibility!
	*/

	public abstract boolean isEmpty();
	/*
	udanax-top.st:47347:ImmuTable methodsFor: 'testing'!
	{BooleanVar} isEmpty
		self subclassResponsibility.!
	*/

	public boolean isEqual(Heaper other) {
		if (other instanceof ImmuTable) {
			ImmuTable o = (ImmuTable) other;
			return contentsEqual(o);
		} else {
			return false;
		}
		/*
		udanax-top.st:47350:ImmuTable methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper}
			other cast: ImmuTable into: [:o |
					^self contentsEqual: o]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	public abstract TableStepper stepper(OrderSpec order);
	/*
	udanax-top.st:47359:ImmuTable methodsFor: 'enumerating'!
	{TableStepper} stepper: order {OrderSpec default: NULL}
		self subclassResponsibility!
	*/

	public ImmuTable asImmuTable() {
		return this;
		/*
		udanax-top.st:47364:ImmuTable methodsFor: 'conversion'!
		{ImmuTable} asImmuTable
			^self!
		*/
	}

	public abstract MuTable asMuTable();
	/*
	udanax-top.st:47368:ImmuTable methodsFor: 'conversion'!
	{MuTable} asMuTable
		self subclassResponsibility!
	*/

	///**
	// * Please use ImmuTable::combineWith instead. 'with' was an innapropriate name
	// * because its use elsewhere (see ImmuSet::with and XuRegion::with) implies that
	// * the argument is a single element to be added, not a collection of elements to
	// * be added.
	// */
	//public ImmuTable with(ImmuTable other) {
	//passe();
	//return combineWith(other);
	///*
	//udanax-top.st:47374:ImmuTable methodsFor: 'smalltalk: passe'!
	//{ImmuTable} with: other {ImmuTable} 
	//	"Please use ImmuTable::combineWith instead. 'with' was an innapropriate name 
	//	because its use elsewhere (see ImmuSet::with and XuRegion::with) implies that 
	//	the argument is a single element to be added, not a collection of elements to 
	//	be added."
	//	self passe. 
	//	^self combineWith: other!
	//*/
	//}

	/**
	 * An empty ImmuTable whose domain space is 'cs'.
	 */
	public static Heaper make(CoordinateSpace cs) {
		return new ImmuTableOnMu((MuTable.make(cs)));
		/*
		udanax-top.st:47391:ImmuTable class methodsFor: 'pseudo constructors'!
		{ImmuTable} make.CoordinateSpace: cs {CoordinateSpace}
			"An empty ImmuTable whose domain space is 'cs'."
			
			^ImmuTableOnMu create: (MuTable make: cs)!
		*/
	}

	public static ImmuTable offsetImmuTable(ImmuTable aTable, Dsp aDsp) {
		return new OffsetImmuTable(aTable, aDsp);
		/*
		udanax-top.st:47396:ImmuTable class methodsFor: 'pseudo constructors'!
		{ImmuTable} offsetImmuTable: aTable {ImmuTable} with: aDsp {Dsp}
			^ OffsetImmuTable create: aTable with: aDsp!
		*/
	}

	/**
	 * (something isKindOf: MuTable)
	 * ifTrue: [^self make.MuTable: something].
	 */
	public static Heaper make(Object something) {
		return make(((CoordinateSpace) something));
		/*
		udanax-top.st:47401:ImmuTable class methodsFor: 'smalltalk: smalltalk defaults'!
		make: something
			"(something isKindOf: MuTable)
				ifTrue: [^self make.MuTable: something]."
			^self make.CoordinateSpace: (something cast: CoordinateSpace)!
		*/
	}
}
