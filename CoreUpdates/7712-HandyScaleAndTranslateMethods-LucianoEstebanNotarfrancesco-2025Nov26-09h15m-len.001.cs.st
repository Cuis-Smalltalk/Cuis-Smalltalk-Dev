'From Cuis7.5 [latest update: #7710] on 26 November 2025 at 9:16:17 am'!

!PlacedMorph methodsFor: 'geometry' stamp: 'len 11/15/2025 22:11:51'!
scaleBy: factor around: center
	| delta |
	delta := center - self morphPosition.
	self translateBy: delta; scaleBy: factor; translateBy: delta negated * factor! !

!PlacedMorph methodsFor: 'geometry' stamp: 'len 11/15/2025 15:28:14'!
translateBy: aPoint
	location := location translatedBy: aPoint.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].
	self redrawNeeded! !

