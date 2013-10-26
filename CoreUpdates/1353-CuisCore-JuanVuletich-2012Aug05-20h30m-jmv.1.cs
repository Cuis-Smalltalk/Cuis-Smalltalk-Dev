'From Cuis 4.0 of 21 April 2012 [latest update: #1351] on 5 August 2012 at 10:00:35 pm'!

!Morph methodsFor: 'layout' stamp: 'jmv 8/5/2012 19:45'!
layoutSubmorphsAndComputeFullBounds
	"Return self. Recompute the layout if necessary."

	"Check senders. Many many not be needed. Others might be just to compute fullBounds, that we hope to elliminate!! Keep those that really need layout. of submorphs"
	self flag: #jmvVer2.

	fullBounds ifNotNil: [ ^self ].
	layoutNeeded ifNil: [ layoutNeeded _ true ].
	layoutNeeded ifFalse: [ ^self ].

	self layoutSubmorphs.
	fullBounds _ self computeFullBounds.
	fullBounds _ bounds.
	layoutNeeded _ false! !

!Morph methodsFor: 'layout' stamp: 'jmv 8/5/2012 19:46'!
someSubmorphPositionOrExtentChanged
	"In some submorph or in self"
	fullBounds ifNil: [ ^self ]. "layout will be recomputed so don't bother"
	fullBounds _ nil.
	layoutNeeded ifNil: [ layoutNeeded _ true ].
	layoutNeeded ifTrue: [ ^self ].
	layoutNeeded _ true.	"layout will be recomputed so don't bother"
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ]! !

!Morph methodsFor: 'updating' stamp: 'jmv 8/5/2012 22:00'!
redrawNeeded
	"Report that the area occupied by this morph should be redrawn."

	self flag: #jmvVer2.
	"Invalidate the appropriate display rectangle... Just ours, or include submorphs if we don't clip.
	Think about it. We don't to know about a specific rectangle... How do we notify our 'observers' (i.e. the possible canvases we end drawn upon)?"
	
	"This method is the only real use of ivar fullBounds, other than senders of #fullBounds"

	layoutNeeded ifNil: [ layoutNeeded _ true ].
	self invalidRect: (layoutNeeded
		ifFalse: [ self morphFullBoundsInWorld ]
		ifTrue: [ self morphBoundsInWorld ])! !

