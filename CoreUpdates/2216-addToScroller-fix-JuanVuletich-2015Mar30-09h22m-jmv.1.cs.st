'From Cuis 4.2 of 25 July 2013 [latest update: #2214] on 30 March 2015 at 9:36:43.688 am'!

!Morph methodsFor: 'initialization' stamp: 'jmv 3/30/2015 09:36'!
inATwoWayScrollPane
	"Answer a two-way scroll pane that allows the user to scroll the receiver in either direction.  It will have permanent scroll bars unless you take some special action."
	"
	(EllipseMorph new morphExtent: 500@270) inATwoWayScrollPane openInHand
	"

	| widget |
	self flag: #jmvVer2.
	widget _ PluggableScrollPane new.
	widget addToScroller: self.
	widget morphExtent: (self morphWidth min: 300 max: 100) @ (self morphHeight min: 150 max: 100).
	widget setScrollDeltas.
	widget color: self color darker darker.
	^widget! !


!PluggableScrollPane methodsFor: 'access' stamp: 'jmv 3/30/2015 09:34'!
addToScroller: aMorph

	scroller
		addMorph: aMorph position: 0@0;
		morphExtent: aMorph morphExtent! !

