'From Cuis 4.0 of 21 April 2012 [latest update: #1358] on 6 August 2012 at 4:03:10 pm'!

!Morph methodsFor: 'drawing' stamp: 'jmv 8/6/2012 15:46'!
clippingBounds
	"Return the bounds to which any submorphs should be clipped if the property is set"
	"Maybe shouldn't exist"
	self flag: #jmvVer2.
	^self innerBounds! !

!Morph methodsFor: 'initialization' stamp: 'jmv 8/6/2012 15:46'!
defaultBounds
"answer the default bounds for the receiver"

	"Maybe shouldn't exist"
	self flag: #jmvVer2.
	^ 0 @ 0 corner: 50 @ 40! !


!ImageMorph methodsFor: 'geometry' stamp: 'jmv 8/6/2012 15:49'!
morphExtent: aPoint
	"Do nothing; my extent is determined by my image Form."

	"A clear case of a morph that shouldn't have an 'extent' ivar..."
	self flag: #jmvVer2.! !

