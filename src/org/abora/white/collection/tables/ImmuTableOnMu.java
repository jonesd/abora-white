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
package org.abora.white.collection.tables;

import java.io.PrintWriter;

import org.abora.white.collection.steppers.TableStepper;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.OrderSpec;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class ImmuTableOnMu extends ImmuTable {
	protected MuTable myMuTable;
	/*
	udanax-top.st:47406:
	ImmuTable subclass: #ImmuTableOnMu
		instanceVariableNames: 'myMuTable {MuTable}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:47410:
	(ImmuTableOnMu getOrMakeCxxClassDescription)
		friends:
	'friend SPTR(ImmuTable) immuTable (MuTable*);
	friend SPTR(ImmuTable) immuTable (CoordinateSpace * cs);
	friend SPTR(ImmuTable) MuTable::asImmuTable ();';
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	/**
	 * use the given Mu to store current value
	 */
	public ImmuTableOnMu(MuTable aMuTable) {
		/* it should be a copy for my exclusive use */
		/* this should only be called from the pseudo constructor or from class methods */
		super();
		myMuTable = aMuTable;
		/*
		udanax-top.st:47419:ImmuTableOnMu methodsFor: 'private: instance creation'!
		create: aMuTable {MuTable}
			"use the given Mu to store current value"
			"it should be a copy for my exclusive use"
			"this should only be called from the pseudo constructor or from class methods"
			super create.
			myMuTable _ aMuTable!
		*/
	}

	public CoordinateSpace coordinateSpace() {
		return myMuTable.coordinateSpace();
		/*
		udanax-top.st:47429:ImmuTableOnMu methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^myMuTable coordinateSpace!
		*/
	}

	public IntegerValue count() {
		return myMuTable.count();
		/*
		udanax-top.st:47433:ImmuTableOnMu methodsFor: 'accessing'!
		{IntegerVar} count
			^myMuTable count!
		*/
	}

	public XnRegion domain() {
		return myMuTable.domain();
		/*
		udanax-top.st:47437:ImmuTableOnMu methodsFor: 'accessing'!
		{XnRegion} domain
			^myMuTable domain!
		*/
	}

	public Heaper fetch(Position key) {
		return myMuTable.fetch(key);
		/*
		udanax-top.st:47441:ImmuTableOnMu methodsFor: 'accessing'!
		{Heaper} fetch: key {Position} 
			^myMuTable fetch: key!
		*/
	}

	public Heaper intFetch(IntegerValue key) {
		return myMuTable.intFetch(key);
		/*
		udanax-top.st:47445:ImmuTableOnMu methodsFor: 'accessing'!
		{Heaper} intFetch: key {IntegerVar}
			^myMuTable intFetch: key!
		*/
	}

	public ScruTable subTable(XnRegion region) {
		return new ImmuTableOnMu(((MuTable) (myMuTable.subTable(region))));
		/*
		udanax-top.st:47448:ImmuTableOnMu methodsFor: 'accessing'!
		{ScruTable} subTable: region {XnRegion} 
			^ImmuTableOnMu create: ((myMuTable subTable: region) cast: MuTable)!
		*/
	}

	public ImmuTable combineWith(ImmuTable other) {
		MuTable newMuTable = (MuTable) myMuTable.copy();
		TableStepper others = other.stepper();
		while (others.hasValue()) {
			newMuTable.atStore(others.position(), others.fetch());
			others.step();
		}
		others.destroy();
		return new ImmuTableOnMu(newMuTable);
		/*
		udanax-top.st:47454:ImmuTableOnMu methodsFor: 'SEF manipulation'!
		{ImmuTable} combineWith: other {ImmuTable}
			| newMuTable {MuTable} others {TableStepper} |
			newMuTable _ myMuTable copy cast: MuTable.
			others _ other stepper.
			[others hasValue] whileTrue:
				[newMuTable at: others position store: others fetch.
				others step].
			others destroy.
			^ImmuTableOnMu create: newMuTable!
		*/
	}

	public ImmuTable without(Position index) {
		MuTable newMuTable = (MuTable) myMuTable.copy();
		newMuTable.wipe(index);
		return new ImmuTableOnMu(newMuTable);
		/*
		udanax-top.st:47465:ImmuTableOnMu methodsFor: 'SEF manipulation'!
		{ImmuTable} without: index {Position}
			| newMuTable {MuTable} |
			newMuTable _ myMuTable copy cast: MuTable.
			newMuTable wipe: index.
			^ ImmuTableOnMu create: newMuTable!
		*/
	}

	public MuTable asMuTable() {
		return (MuTable) myMuTable.copy();
		/*
		udanax-top.st:47474:ImmuTableOnMu methodsFor: 'conversion'!
		{MuTable} asMuTable
			^myMuTable copy cast: MuTable!
		*/
	}

	public boolean includesIntKey(IntegerValue aKey) {
		return myMuTable.includesIntKey(aKey);
		/*
		udanax-top.st:47480:ImmuTableOnMu methodsFor: 'testing'!
		{BooleanVar} includesIntKey: aKey {IntegerVar}
			^ myMuTable includesIntKey: aKey!
		*/
	}

	public boolean includesKey(Position aKey) {
		return myMuTable.includesKey(aKey);
		/*
		udanax-top.st:47483:ImmuTableOnMu methodsFor: 'testing'!
		{BooleanVar} includesKey: aKey {Position}
			^ myMuTable includesKey: aKey!
		*/
	}

	public boolean isEmpty() {
		return myMuTable.isEmpty();
		/*
		udanax-top.st:47486:ImmuTableOnMu methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^myMuTable isEmpty!
		*/
	}

	public TableStepper stepper(OrderSpec order) {
		return myMuTable.copy().stepper(order
		/* making the copy prevents anyone from getting access to the array through TableStepper array */
		);
		/*
		udanax-top.st:47491:ImmuTableOnMu methodsFor: 'enumerating'!
		{TableStepper} stepper: order {OrderSpec default: NULL}
			^myMuTable copy stepper: order	"making the copy prevents anyone from getting access to the array through TableStepper array"!
		*/
	}

	public Heaper theOne() {
		return myMuTable.theOne();
		/*
		udanax-top.st:47494:ImmuTableOnMu methodsFor: 'enumerating'!
		{Heaper} theOne
			^ myMuTable theOne!
		*/
	}

	public MuTable getMuTable() {
		return myMuTable;
		/*
		udanax-top.st:47499:ImmuTableOnMu methodsFor: 'private: accessing'!
		{MuTable} getMuTable
			^myMuTable!
		*/
	}

	public ScruTable emptySize(IntegerValue size) {
		return new ImmuTableOnMu(((MuTable) (myMuTable.emptySize(size))));
		/*
		udanax-top.st:47504:ImmuTableOnMu methodsFor: 'creation'!
		{ScruTable} emptySize: size {IntegerVar}
			^ImmuTableOnMu create: ((myMuTable emptySize: size) cast: MuTable)!
		*/
	}

	public XnRegion runAt(Position key) {
		return myMuTable.runAt(key);
		/*
		udanax-top.st:47510:ImmuTableOnMu methodsFor: 'runs'!
		{XnRegion} runAt: key {Position} 
			^myMuTable runAt: key!
		*/
	}

	public XnRegion runAtInt(IntegerValue key) {
		return myMuTable.runAtInt(key);
		/*
		udanax-top.st:47514:ImmuTableOnMu methodsFor: 'runs'!
		{XnRegion} runAtInt: key {IntegerVar}
			^myMuTable runAtInt: key!
		*/
	}

	public void printOn(PrintWriter oo) {
		oo.print(getClass().getName());
		oo.print("(");
		oo.print(myMuTable);
		oo.print(")");
		/*
		udanax-top.st:47519:ImmuTableOnMu methodsFor: 'printing'!
		{void} printOn: oo {ostream reference}
			oo << self getCategory name << '(' << myMuTable << ')'!
		*/
	}

	public ImmuTableOnMu(Rcvr receiver) {
		super(receiver);
		myMuTable = (MuTable) receiver.receiveHeaper();
		/*
		udanax-top.st:47525:ImmuTableOnMu methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			myMuTable _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(myMuTable);
		/*
		udanax-top.st:47529:ImmuTableOnMu methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: myMuTable.!
		*/
	}
}
