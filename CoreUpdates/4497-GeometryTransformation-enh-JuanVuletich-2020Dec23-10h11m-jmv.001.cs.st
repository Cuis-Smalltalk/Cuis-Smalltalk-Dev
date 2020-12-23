'From Cuis 5.0 [latest update: #4494] on 23 December 2020 at 10:12:26 am'!

!GeometryTransformation methodsFor: 'testing' stamp: 'jmv 12/17/2020 14:43:53'!
doesMirror
	"Return true if the receiver mirrors points around some rect."

	^false! !


!AffineTransformation methodsFor: 'testing' stamp: 'jmv 12/21/2020 17:06:08'!
doesMirror
	"Return true if the receiver mirrors points around some rect."

	| f |
	f _ self a11 * self a22.
	^ f = 0.0
		ifTrue: [ self a12 * self a21 > 0.0]
		ifFalse: [ f < 0.0 ]! !

!AffineTransformation methodsFor: 'transforming rects' stamp: 'jmv 12/21/2020 18:31:50'!
boundsOfInverseTransformOf: aRectangle
	"Internalize aRectangle, and find a bounding rectangle with horizontal 
		and vertical bounds"

	^Rectangle encompassing: (aRectangle corners collect: [ :pt |
		self inverseTransform: pt ])! !


!MorphicTranslation methodsFor: 'transforming rects' stamp: 'jmv 12/21/2020 18:35:37'!
boundsOfInverseTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^aRectangle translatedBy: self translation negated! !

