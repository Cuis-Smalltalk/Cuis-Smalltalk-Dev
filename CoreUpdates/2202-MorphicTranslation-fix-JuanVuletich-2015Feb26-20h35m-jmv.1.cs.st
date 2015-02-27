'From Cuis 4.2 of 25 July 2013 [latest update: #2198] on 26 February 2015 at 8:41:48.276381 pm'!

!MorphicTranslation commentStamp: '<historical>' prior: 0!
Can replace an AffineTransformation when there is only a translation applied (no scale change, no rotation, no skew).

Note: Methods that modify an instance answer the result. The result might be the receiver itself, or it might be a new instance. Therefore:
- Don't assume the receiver will be modified. Always take the result.
- Don't assume the answer will be a new instance. The receiver might be modified!! (make a copy if needed)

For consistency with AffineTransformation, always answer Float coordinates (Fraction coordinates are not handled by BitBlt or VectorEngine).!


!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 2/26/2015 20:35'!
externalizeDelta: aPoint
	"Externalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint asFloatPoint! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 2/26/2015 20:36'!
externalizeScalar: aNumber
	"Externalize a distance (without a direction).
	 We keep scale of stuff."

	^aNumber asFloat! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 2/26/2015 20:36'!
internalizeDelta: aPoint
	"Internalize a distance vector. A distance is not a position. It is a magnitude with a direction.
	It is usually used as a delta to be added to a position to obtain some other position.
	We keep scale of stuff."

	^aPoint asFloatPoint! !

!MorphicTranslation methodsFor: 'converting coordinates' stamp: 'jmv 2/26/2015 20:36'!
internalizeScalar: aNumber
	"Internalize a distance (without a direction). 
	We keep scale of stuff."

	^aNumber asFloat! !

!MorphicTranslation methodsFor: 'private' stamp: 'jmv 2/26/2015 20:35'!
setTranslation: aPoint
	"private. Set the raw offset in the receiver"

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x asFloat.
	deltaY _ pt y asFloat! !

!MorphicTranslation methodsFor: 'modifying' stamp: 'jmv 2/26/2015 20:35'!
withTranslation: aPoint
	"set an offset in the receiver

	Answer the modified object. I this implementation is self, but some classes of transformations,
	more restricted ones (like a possible NullTransformation or such) could require the creation of a new, more general instance.
	Senders should always use the returned object, but not assume it is a new one: it could also be the receiver itself."

	| pt |
	pt _ aPoint asPoint.
	deltaX _ pt x asFloat.
	deltaY _ pt y asFloat.
	^self! !

!MorphicTranslation methodsFor: 'initialization' stamp: 'jmv 2/26/2015 20:35'!
initialize
	"Initialize the receiver to the identity transformation (e.g., not affecting points)"
	deltaX _ 0.0.
	deltaY _ 0.0! !

