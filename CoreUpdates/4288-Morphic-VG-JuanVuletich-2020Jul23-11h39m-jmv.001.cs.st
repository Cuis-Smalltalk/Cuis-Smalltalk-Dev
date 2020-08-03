'From Cuis 5.0 [latest update: #4284] on 23 July 2020 at 11:43:33 am'!

!HandMorph methodsFor: 'geometry' stamp: 'jmv 7/23/2020 11:43:17'!
displayFullBounds	
	"Current full bounds of hand and carried submorphs.
	Submorphs might have never been drawn at this hand position (this is usually the case when dragging morphs around)."
	| fullBounds |
	fullBounds _ self displayBounds.
	self submorphsDo: [ :m |
		fullBounds _ fullBounds quickMerge: m displayFullBounds].
	(self valueOfProperty: #lastPos)
		ifNotNil: [ :lastPos |						"When already carrying morphs around."
			fullBounds _ fullBounds translatedBy: self morphPosition-lastPos.
			fullBounds _ fullBounds quickMerge: self displayBounds ].
	^fullBounds! !

!methodRemoval: HandMorph #submorphsDisplayFullBounds stamp: 'jmv 7/23/2020 11:43:27'!
HandMorph removeSelector: #submorphsDisplayFullBounds!
