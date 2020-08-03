'From Cuis 5.0 [latest update: #4292] on 24 July 2020 at 11:21:45 am'!

!Morph methodsFor: 'geometry' stamp: 'jmv 7/24/2020 11:16:52'!
displayBoundsOrBogus
	"Never answer nil. A bogus value will trigger a draw operation, and bounds will later be corrected."
	^self displayBounds ifNil: [
		Rectangle center: (self externalizeToWorld: 0@0) extent: 2@2 ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 7/24/2020 11:17:30'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self setProperty: #needsRedraw toValue: true.
	self allOwnersDo: [ :m | m setProperty: #submorphNeedsRedraw toValue: true ].
	self invalidateDisplayRect: self displayBoundsOrBogus from: nil.
	self submorphsDrawingOutsideReverseDo: [ :m | m redrawNeeded ]! !

