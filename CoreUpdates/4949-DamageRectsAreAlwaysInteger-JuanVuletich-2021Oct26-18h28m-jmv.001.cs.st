'From Cuis 5.0 [latest update: #4948] on 26 October 2021 at 6:38:33 pm'!

!Morph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:36:22'!
displayFullBounds
	"Answer a rectangle that completely bounds receiver and submorphs when last drawn 
		(if the world was to be fully drawn, i.e. only to be used on a Canvas on the full world).
	Integer pixel coordinates!!"

	| answer |
	answer _ self basicDisplayBounds.
	answer ifNil: [ ^nil ].
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m |
			answer _ answer quickMerge: m displayFullBounds ]].
	^answer! !


!HandMorph methodsFor: 'geometry' stamp: 'jmv 10/26/2021 18:34:48'!
displayFullBoundsForPatch
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around).
	Integer pixel coordinates!!"

	| handBounds fullBounds submorphBounds |
	handBounds _ self basicDisplayBounds.
	submorphBounds _ nil.
	self submorphsDo: [ :m |
		m displayFullBounds ifNotNil: [ :b |
			submorphBounds _ b quickMerge: submorphBounds ]].
	fullBounds _ handBounds quickMerge: submorphBounds.
	lastPosition
		ifNotNil: [ :lastPos |						"When already carrying morphs around. Update rectangle to current hand position."
			fullBounds _ fullBounds quickMerge: (submorphBounds translatedBy: self morphPosition-lastPos) ].
	^fullBounds encompassingIntegerRectangle! !


!DamageRecorder methodsFor: 'private' stamp: 'jmv 10/26/2021 18:37:56'!
pvtInnerRecordInvalidRect: requestedRect for: aRootMorph
	"Record the given rectangle in my damage list, a list of rectangular areas of the display that should be redraw on the next display cycle."

	aRootMorph ifNotNil: [
		(damageByRoot at: aRootMorph
			ifPresent: [ :r | r updateMerging: requestedRect]
			ifAbsent: [ damageByRoot at: aRootMorph put: requestedRect copy ]) ]
		ifNil: [otherDamage add: requestedRect copy].! !

!methodRemoval: Rectangle #encompassingIntegerRectangleX stamp: 'jmv 10/26/2021 18:38:02'!
Rectangle removeSelector: #encompassingIntegerRectangleX!
