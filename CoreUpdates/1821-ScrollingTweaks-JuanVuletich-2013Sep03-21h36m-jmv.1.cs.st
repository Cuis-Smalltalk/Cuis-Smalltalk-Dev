'From Cuis 4.2 of 25 July 2013 [latest update: #1820] on 3 September 2013 at 9:37:11.980928 pm'!

!Morph methodsFor: 'initialization' stamp: 'jmv 9/3/2013 19:18'!
inATwoWayScrollPane
	"Answer a two-way scroll pane that allows the user to scroll the receiver in either direction.  It will have permanent scroll bars unless you take some special action."

	| widget |
	self flag: #jmvVer2.
	widget _ PluggableScrollPane new.
	widget addToScroller: self.
	widget morphExtent: (self morphWidth min: 300 max: 100) @ (self morphHeight min: 150 max: 100).
	widget setScrollDeltas.
	widget color: self color darker darker.
	^widget! !


!ScrollBar methodsFor: 'geometry' stamp: 'jmv 9/3/2013 21:34'!
freeSliderRoom
	"Answer the length or height of the free slider area, i.e. substract the slider itself.
	If we are really too short of room, lie a little bit. Answering at least 4, even when the
	free space might be actually negative, makes the scrollbar somewhat usable."

	^ ((self isHorizontal
		ifTrue: [ extent x - slider morphWidth]
		ifFalse: [ extent y - slider morphHeight])
			- (borderWidth * 2) - (self buttonExtent * 2)) max: 4! !

