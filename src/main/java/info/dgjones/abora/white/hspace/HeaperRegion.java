/*
 * Abora-Gold
 * Part of the Abora hypertext project: http://www.abora.org
 * Copyright 2003 David G Jones
 * 
 * Translated from Udanax-Gold source code: http://www.udanax.com
 * Copyright 1979-1999 Udanax.com. All rights reserved
 */
package info.dgjones.abora.white.hspace;

import info.dgjones.abora.white.collection.sets.ImmuSet;
import info.dgjones.abora.white.collection.sets.ScruSet;
import info.dgjones.abora.white.rcvr.Rcvr;
import info.dgjones.abora.white.rcvr.Xmtr;
import info.dgjones.abora.white.spaces.basic.CoordinateSpace;
import info.dgjones.abora.white.spaces.basic.OrderSpec;
import info.dgjones.abora.white.spaces.basic.XnRegion;
import info.dgjones.abora.white.spaces.unordered.HeaperAsPosition;
import info.dgjones.abora.white.spaces.unordered.HeaperSpace;
import info.dgjones.abora.white.spaces.unordered.SetRegion;

public class HeaperRegion extends SetRegion {
	/*
	udanax-top.st:70180:
	SetRegion subclass: #HeaperRegion
		instanceVariableNames: ''
		classVariableNames: ''
		poolDictionaries: ''
		category: 'Xanadu-hspace'!
	*/
	/*
	udanax-top.st:70184:
	(HeaperRegion getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/
	/*
	udanax-top.st:70217:
	HeaperRegion class
		instanceVariableNames: ''!
	*/
	/*
	udanax-top.st:70220:
	(HeaperRegion getOrMakeCxxClassDescription)
		attributes: ((Set new) add: #CONCRETE; add: #NOT.A.TYPE; add: #COPY; yourself)!
	*/

	public CoordinateSpace coordinateSpace() {
		return HeaperSpace.make();
		/*
		udanax-top.st:70189:HeaperRegion methodsFor: 'accessing'!
		{CoordinateSpace} coordinateSpace
			^HeaperSpace make!
		*/
	}

	public XnRegion makeNew(boolean isComplement, ImmuSet positions) {
		return new HeaperRegion(isComplement, positions);
		/*
		udanax-top.st:70194:HeaperRegion methodsFor: 'protected: protected'!
		{XnRegion} makeNew: isComplement {BooleanVar} with: positions {ImmuSet of: Position}
			^HeaperRegion create: isComplement with: positions!
		*/
	}

	public boolean isEnumerable(OrderSpec order) {
		return false;
		/*
		udanax-top.st:70199:HeaperRegion methodsFor: 'testing'!
		{BooleanVar} isEnumerable: order {OrderSpec default: NULL}
			^false!
		*/
	}

	public HeaperRegion(boolean isComplement, ImmuSet positions) {
		super(isComplement, positions);
		/*
		udanax-top.st:70205:HeaperRegion methodsFor: 'creation'!
		create: isComplement {BooleanVar} with: positions {ImmuSet of: HeaperAsPosition}
			super create: isComplement with: positions!
		*/
	}

	public HeaperRegion(Rcvr receiver) {
		super(receiver);
		/*
		udanax-top.st:70210:HeaperRegion methodsFor: 'generated:'!
		create.Rcvr: receiver {Rcvr}
			super create.Rcvr: receiver.!
		*/
	}

	public void sendSelfTo(Xmtr xmtr) {
		super.sendSelfTo(xmtr);
		/*
		udanax-top.st:70213:HeaperRegion methodsFor: 'generated:'!
		{void} sendSelfTo: xmtr {Xmtr}
			super sendSelfTo: xmtr.!
		*/
	}

	public static SetRegion allHeaperAsPositions() {
		return new HeaperRegion(true, ImmuSet.make());
		/*
		udanax-top.st:70225:HeaperRegion class methodsFor: 'pseudo constructors'!
		{SetRegion} allHeaperAsPositions
			^HeaperRegion create: true with: ImmuSet make!
		*/
	}

	public static SetRegion make() {
		return new HeaperRegion(false, ImmuSet.make());
		/*
		udanax-top.st:70228:HeaperRegion class methodsFor: 'pseudo constructors'!
		{SetRegion} make
			^HeaperRegion create: false with: ImmuSet make!
		*/
	}

	public static SetRegion make(HeaperAsPosition heaper) {
		return new HeaperRegion(false, (ImmuSet.make().with(heaper)));
		/*
		udanax-top.st:70231:HeaperRegion class methodsFor: 'pseudo constructors'!
		{SetRegion} make.HeaperAsPosition: heaper {HeaperAsPosition}
			^HeaperRegion create: false with: (ImmuSet make with: heaper)!
		*/
	}

	public static SetRegion make(ScruSet heapers) {
		return new HeaperRegion(false, heapers.asImmuSet());
		/*
		udanax-top.st:70234:HeaperRegion class methodsFor: 'pseudo constructors'!
		{SetRegion} make.ScruSet: heapers {ScruSet of: HeaperAsPosition}
			^HeaperRegion create: false with: heapers asImmuSet!
		*/
	}
}
