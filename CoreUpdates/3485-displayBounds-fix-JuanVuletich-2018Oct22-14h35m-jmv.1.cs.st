'From Cuis 5.0 of 7 November 2016 [latest update: #3484] on 22 October 2018 at 2:36:01 pm'!

!MorphicTranslation methodsFor: 'transforming rects' stamp: 'jmv 10/22/2018 14:32:45'!
displayBoundsOfTransformOf: aRectangle
	"Externalize aRectangle, and find a bounding rectangle with horizontal 
	and vertical bounds and integer coordinates (i.e. adisplayBounds)."

	^(aRectangle translatedBy: self translation) rounded! !

