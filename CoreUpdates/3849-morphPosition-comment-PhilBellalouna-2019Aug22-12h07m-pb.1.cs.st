'From Cuis 5.0 [latest update: #3840] on 22 August 2019 at 12:10:43 pm'!

!Morph methodsFor: 'geometry' stamp: 'pb 8/22/2019 12:08:01'!
morphPosition: aPoint
	"Change the position of this morph. Argument is in owner's coordinates."
	(location isTranslation: aPoint) ifTrue: [ "Null change"
		^ self ].
	"Invalidate the rectangle at the old position..."
	self redrawNeeded.
	location _ location withTranslation: aPoint.
	"... and the new position"
	self redrawNeeded.
	owner ifNotNil: [ owner someSubmorphPositionOrExtentChanged ].! !

