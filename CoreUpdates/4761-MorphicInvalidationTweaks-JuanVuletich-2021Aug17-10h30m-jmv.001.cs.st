'From Cuis 5.0 [latest update: #4760] on 17 August 2021 at 10:30:58 am'!

!Morph methodsFor: 'updating' stamp: 'jmv 8/17/2021 09:16:29'!
invalidateBounds
	"Report that the area occupied by this morph and submorphs should be redrawn, 
	but most likely we are not going to be redrawn on it, or maybe we'll not be fully visible. See senders."

	self displayFullBounds ifNotNil: [ :r |
		"outsetBy: 1 due to anti aliasing in VectorCanvas"
		self invalidateDisplayRect: (r outsetBy: 1) for: nil ].
! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/17/2021 09:16:15'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn.
	Usually because we changed in some way."

	self needsRedraw: true.
	self allOwnersDo: [ :m | m submorphNeedsRedraw: true ].
	"outsetBy: 1 due to anti aliasing in VectorCanvas"
	self invalidateDisplayRect: (self displayBoundsOrBogus outsetBy: 1) for: self.
	(self submorphsMightProtrude and: [self clipsSubmorphsReally not]) ifTrue: [
		self submorphsReverseDo: [ :m | m redrawNeeded ]].! !


!MorphicCanvas methodsFor: 'morphic' stamp: 'jmv 8/17/2021 10:06:42'!
drawCurrentAsError
	"The morph (or one of its submorphs) had an error in its drawing method."
	| r w |
	r _ currentMorph morphLocalBoundsForError.
	w _ r extent > `50@50`
		ifTrue: [ 6 ]
		ifFalse: [ 2 ].
	self
		frameAndFillRectangle: r
		fillColor: `Color red`
		borderWidth: w
		borderColor: `Color yellow`.
	self line: r topLeft-0.5 to: r bottomRight-0.5 width: w color: `Color yellow`.
	self line: r topRight-0.5 to: r bottomLeft-0.5 width: w color: `Color yellow`.	
	currentMorph displayBoundsSetFrom: self.! !

