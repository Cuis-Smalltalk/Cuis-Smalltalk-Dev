'From Cuis 5.0 [latest update: #4716] on 30 July 2021 at 3:34:45 pm'!

!KernelMorph methodsFor: 'geometry testing' stamp: 'jmv 7/30/2021 10:53:34'!
wantsContour
	"Kernel morphs don't usually need contour"

	^false! !


!WidgetMorph methodsFor: 'geometry testing' stamp: 'jmv 7/30/2021 10:54:19'!
wantsContour
	"Widget morphs don't usually need contour"

	^false! !


!Morph methodsFor: 'geometry' stamp: 'jmv 7/30/2021 11:00:12'!
displayBounds
	"Private for framework use.
	At time of last draw on world canvas. nil if unknown.
	Only to be used for the world canvas (i.e. this ivar is not to represent our bounds in any other context).
	Rectangle is clipped appropiately if some owner clips us.
	Integer pixel coordinates!!"

	self visible ifFalse: [ ^nil ].
	^ privateDisplayBounds! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/30/2021 11:00:34'!
displayBoundsOrBogus
	"Never answer nil. A bogus value will trigger a draw operation, and bounds will later be corrected.
	Integer pixel coordinates!!"

	^self displayBounds ifNil: [
		Rectangle center: (self externalizeToWorld: `0@0`) extent: `2@2` ]! !

!Morph methodsFor: 'geometry' stamp: 'jmv 7/30/2021 15:34:05'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Integer pixel coordinates!!"

	| answer |
	answer _ self displayBounds.
	answer ifNil: [ ^nil ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m |
			answer _ answer quickMerge: m displayFullBounds ]].
	^answer encompassingIntegerRectangle! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/30/2021 15:34:19'!
displayFullBounds	
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Include also the rectangle updated to current hand position."
			fullBounds _ fullBounds quickMerge: (fullBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !


!InnerPluggableMorph methodsFor: 'geometry' stamp: 'jmv 7/30/2021 10:49:50'!
adjustExtent
	"Morphs intended to be included in PluggableScrollPanes as scrolled content might need to adjust their extent."

	self subclassResponsibility! !

