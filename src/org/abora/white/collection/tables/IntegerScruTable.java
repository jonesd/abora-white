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

import org.abora.white.collection.steppers.TableStepper;
import org.abora.white.rcvr.Rcvr;
import org.abora.white.rcvr.Xmtr;
import org.abora.white.spaces.basic.CoordinateSpace;
import org.abora.white.spaces.basic.OrderSpec;
import org.abora.white.spaces.basic.Position;
import org.abora.white.spaces.basic.XnRegion;
import org.abora.white.spaces.integers.IntegerSpace;
import org.abora.white.value.IntegerValue;
import org.abora.white.xpp.basic.Heaper;

public class IntegerScruTable extends ScruTable {
	protected IntegerTable tableToScru;
	/*
	udanax-top.st:47661:
	ScruTable subclass: #IntegerScruTable
		instanceVariableNames: 'tableToScru {IntegerTable}'
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-Collection-Tables'!
	*/
	/*
	udanax-top.st:47665:
	(IntegerScruTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:47774:
	IntegerScruTable class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:47777:
	(IntegerScruTable getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	public CoordinateSpace coordinateSpace() {
		return IntegerSpace.make();
		/*
		udanax-top.st:47670:IntegerScruTable methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			
			^IntegerSpace make!
		*/
	}

	public IntegerValue count() {
		return tableToScru.count();
		/*
		udanax-top.st:47674:IntegerScruTable methodsFor: 'accessing'!
		{IntegerVar} count
			^tableToScru count!
		*/
	}

	public XnRegion domain() {
		return tableToScru.domain();
		/*
		udanax-top.st:47678:IntegerScruTable methodsFor: 'accessing'!
		{XnRegion} domain
			^ tableToScru domain!
		*/
	}

	public Heaper fetch(Position key) {
		return tableToScru.fetch(key);
		/*
		udanax-top.st:47682:IntegerScruTable methodsFor: 'accessing'!
		{Heaper} fetch: key {Position} 
			^ tableToScru fetch: key!
		*/
	}

	public Heaper intFetch(IntegerValue key) {
		return tableToScru.intFetch(key);
		/*
		udanax-top.st:47686:IntegerScruTable methodsFor: 'accessing'!
		{Heaper} intFetch: key {IntegerVar}
			^ tableToScru intFetch: key!
		*/
	}

	public ScruTable subTable(XnRegion reg) {
		return tableToScru.subTable(reg);
		/*
		udanax-top.st:47689:IntegerScruTable methodsFor: 'accessing'!
		{ScruTable} subTable: reg {XnRegion} 
			^ tableToScru subTable: reg!
		*/
	}

	/**
	 * Return a table which contains the intersection of this table's domain and the
	 * domain specified by the enclosure.
	 */
	public ScruTable subTableBetween(IntegerValue start, IntegerValue stop) {
		return tableToScru.offsetSubTableBetween(start, stop, start);
		/*
		udanax-top.st:47693:IntegerScruTable methodsFor: 'accessing'!
		{ScruTable} subTableBetween: start {IntegerVar} with: stop {IntegerVar}
			"Return a table which contains the intersection of this table's domain and the 
			domain specified by the enclosure."
			^ tableToScru offsetSubTableBetween: start with: stop with: start!
		*/
	}

	public ScruTable copy() {
		return new IntegerScruTable(((IntegerTable) (tableToScru.copy())));
		/*
		udanax-top.st:47701:IntegerScruTable methodsFor: 'creation'!
		{ScruTable} copy
			^ IntegerScruTable create: ((tableToScru copy) quickCast: IntegerTable)!
		*/
	}

	public IntegerScruTable(IntegerTable fromTable) {
		super();
		tableToScru = fromTable;
		/*
		udanax-top.st:47704:IntegerScruTable methodsFor: 'creation'!
		create: fromTable {IntegerTable}
			super create.
			tableToScru _ fromTable!
		*/
	}

	public ScruTable emptySize(IntegerValue size) {
		return new IntegerScruTable(((IntegerTable) (tableToScru.emptySize(size))));
		/*
		udanax-top.st:47708:IntegerScruTable methodsFor: 'creation'!
		{ScruTable} emptySize: size {IntegerVar}
			^ IntegerScruTable create.IntegerVar: ((tableToScru emptySize: size) quickCast: IntegerTable)!
		*/
	}

	public XnRegion runAt(Position key) {
		return tableToScru.runAt(key);
		/*
		udanax-top.st:47714:IntegerScruTable methodsFor: 'runs'!
		{XnRegion} runAt: key {Position} 
			^ tableToScru runAt: key!
		*/
	}

	public XnRegion runAtInt(IntegerValue key) {
		return tableToScru.runAtInt(key);
		/*
		udanax-top.st:47718:IntegerScruTable methodsFor: 'runs'!
		{XnRegion} runAtInt: key {IntegerVar}
			^ tableToScru runAtInt: key!
		*/
	}

	public int actualHashForEqual() {
		return getClass().hashCode() + tableToScru.hashForEqual();
		/*
		udanax-top.st:47723:IntegerScruTable methodsFor: 'testing'!
		{UInt32} actualHashForEqual
			^#cat.U.IntegerScruTable hashForEqual + tableToScru hashForEqual!
		*/
	}

	public boolean includesIntKey(IntegerValue aKey) {
		return tableToScru.includesIntKey(aKey);
		/*
		udanax-top.st:47726:IntegerScruTable methodsFor: 'testing'!
		{BooleanVar} includesIntKey: aKey {IntegerVar}
			^ tableToScru includesIntKey: aKey!
		*/
	}

	public boolean includesKey(Position aKey) {
		return tableToScru.includesKey(aKey);
		/*
		udanax-top.st:47729:IntegerScruTable methodsFor: 'testing'!
		{BooleanVar} includesKey: aKey {Position}
			^ tableToScru includesKey: aKey!
		*/
	}

	public boolean isEmpty() {
		return tableToScru.isEmpty();
		/*
		udanax-top.st:47732:IntegerScruTable methodsFor: 'testing'!
		{BooleanVar} isEmpty
			^ tableToScru isEmpty!
		*/
	}

	public boolean isEqual(Heaper other) {
		if (other instanceof IntegerScruTable) {
			IntegerScruTable ist = (IntegerScruTable) other;
			return ist.innerTable().isEqual(tableToScru);
		} else {
			return false;
		}
		/*
		udanax-top.st:47735:IntegerScruTable methodsFor: 'testing'!
		{BooleanVar} isEqual: other {Heaper} 
			other
				cast: IntegerScruTable into: [:ist |
					^ist innerTable isEqual: tableToScru]
				others: [^false].
			^ false "compiler fodder"!
		*/
	}

	public TableStepper stepper(OrderSpec order) {
		return tableToScru.stepper(order);
		/*
		udanax-top.st:47745:IntegerScruTable methodsFor: 'enumerating'!
		{TableStepper} stepper: order {OrderSpec default: NULL}
			^ tableToScru stepper: order!
		*/
	}

	public ImmuTable asImmuTable() {
		return tableToScru.asImmuTable();
		/*
		udanax-top.st:47750:IntegerScruTable methodsFor: 'conversion'!
		{ImmuTable} asImmuTable
			^ tableToScru asImmuTable!
		*/
	}

	public MuTable asMuTable() {
		return tableToScru.copy().asMuTable();
		/*
		udanax-top.st:47754:IntegerScruTable methodsFor: 'conversion'!
		{MuTable} asMuTable
			^ tableToScru copy asMuTable!
		*/
	}

	public ScruTable innerTable() {
		return tableToScru;
		/*
		udanax-top.st:47760:IntegerScruTable methodsFor: 'private: private'!
		{ScruTable} innerTable
			^tableToScru!
		*/
	}

	public IntegerScruTable(Rcvr receiver) {
		super(receiver);
		tableToScru = (IntegerTable) receiver.receiveHeaper();
		/*
		udanax-top.st:47765:IntegerScruTable methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.
			tableToScru _ receiver receiveHeaper.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		xmtr.sendHeaper(tableToScru);
		/*
		udanax-top.st:47769:IntegerScruTable methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.
			xmtr sendHeaper: tableToScru.!
		*/
	}

	public static Heaper make(IntegerTable fromTable) {
		return new IntegerScruTable(fromTable);
		/*
		udanax-top.st:47782:IntegerScruTable class methodsFor: 'pseudo constructors'!
		{ScruTable} make: fromTable {IntegerTable}
			^ IntegerScruTable create: fromTable!
		*/
	}
}
